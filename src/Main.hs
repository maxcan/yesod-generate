{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | main entry point for the generators.
--
--   in the future, we may support multiple generators.  
--   we dont now
module Main where

import Control.Monad.Error hiding (mapM_)
import Control.Monad.Error.Class
import Control.Monad.Trans (liftIO)
-- import System.Directory
import System.Environment (getArgs)
import Foundation
import qualified Data.Text as DT 
import qualified Data.Text.Lazy as LT 
-- import Text.ParserCombinators.Parsec
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import qualified Data.Char as DC
import System.IO (putStrLn, print)
import Text.Shakespeare.Text hiding (toText)
import Filesystem.Path.CurrentOS (fromText, toText)
import Filesystem
import Filesystem.Path hiding (concat)

routesFp = "config/routes" :: FilePath
modelsFp = "config/models" :: FilePath

type ErrT = ErrorT Text IO
main :: IO ()
main = do
  res <- runErrorT main_
  print $ show res
  return ()

main_ :: ErrT ()
main_ = do 
  -- sanity check that files exist
  checkFileExists routesFp
  checkFileExists modelsFp
  -- check for cabal file:
  cabalFp <- do
    dirContents <- liftIO $ listDirectory "."
    case filter (flip hasExtension "cabal") dirContents of
      [f] -> return f
      []  -> throwError "no cabal file found"
      _   -> throwError "multiple cabal files found"
  -- check that there isn't already a handler file with this name:
  -- check args
  args <- liftIO getArgs
  case args of
    "model" : _modelName : _fields -> do
      let modelNameUpper = DT.pack _modelName 
          fields = map DT.pack _fields
      
      unless (DC.isUpper $ DT.head modelNameUpper) 
        $ throwError "model name must be uppercase"
      liftIO $ putStrLn "checking model name" -- _DEBUG
      checkModelName modelNameUpper
      liftIO $ putStrLn "aobut ot make fields" -- _DEBUG
      fields <- makeFields fields
      liftIO $ putStrLn "MADE fields" -- _DEBUG
      when (length fields < 1) $ throwError $ "Need at least one field!"
      liftIO $ do putStrLn "Checking handler directory"
                  createDirectory True "Handler"
      let handlerFp = fromText $ "Handler/" ++ modelNameUpper ++ ".hs"
      handlerExists <- liftIO $ isFile handlerFp
      when handlerExists $ throwError "Handler File already exists, cannot continue"
      ------------------------------------------------------------------------
      --  FIRE THE MISSILES - This is where we pass the point of no return  -- 
      ------------------------------------------------------------------------
      addHandlerModuleToCabalFile cabalFp $ "Handler." ++ modelNameUpper
      addModelToModelsFile modelsFp modelNameUpper fields
      genHandlerFile handlerFp modelNameUpper fields
      liftIO $ putStrLn "Finished Successfully!"
    m : _ -> throwError $ "No generator for: " ++ DT.pack m
    [] -> throwError $ "Need some arguments!  Toss me a freakin bone here"
  
addModelToModelsFile :: FilePath -> Text -> [FieldDesc] -> ErrT ()
addModelToModelsFile fp modelNameUpper flds = liftIO $ do
  createBackupCopy fp
  appendTextFile fp $ "\n" ++ DT.intercalate "\n" (modelNameUpper : map fdToModel flds)
 where
  fdToModel fd =
    "  " ++ fdName fd ++ " " ++ typeForFieldType (fdType fd) ++ 
    if fdNullable fd then " Maybe" else ""

genHandlerFile
  :: FilePath
  -> Text
  -> [FieldDesc]
  -> ErrT ()
genHandlerFile fp modelNameUpper_ flds = do
  generatedForm <- DT.unpack <$> formText modelNameUpper_ flds
  liftIO $ writeTextFile fp $(codegenFile "codegen/generic-handler.cg")
 where
  modelNameUpper = DT.unpack modelNameUpper_
  modelNameLower = DT.unpack $ 
    DT.cons (DC.toLower $ DT.head modelNameUpper_) (DT.tail modelNameUpper_)
  

addHandlerModuleToCabalFile :: FilePath -> Text -> ErrT ()
addHandlerModuleToCabalFile fp moduleName = do
  allLines <- liftIO $ lines <$> readTextFile fp
  case break ("other-modules" `DT.isInfixOf`) allLines of
    ([],_) -> throwError "malformed cabal file (probably missing other-modules field)"
    (_,[]) -> throwError "malformed cabal file (probably missing other-modules field)"
    (prevLines, otherModulesLine:restOfLines) -> liftIO $ do
      createBackupCopy fp
      let moduleLine = "                     " ++ moduleName
      writeTextFile fp . DT.intercalate "\n" $  
        prevLines ++ (otherModulesLine : moduleLine : restOfLines)
        
-- genRoutes :: Text -> Text -> Text
genRoutes modelNameUpper_ = $(codegenFile "codegen/routes.cg")
 where
  modelNameUpper = DT.unpack modelNameUpper_
  modelNameLower = DT.unpack $ 
    DT.cons (DC.toLower $ DT.head modelNameUpper_) (DT.tail modelNameUpper_)

addRoutes 
  :: FilePath -- ^ usually config/routes
  -> Text     -- ^ the uppercase Model Name
  -> ErrT ()
addRoutes fp modelNameUpper = liftIO $ do
  createBackupCopy fp
  appendTextFile fp "\n"
  appendTextFile fp $ genRoutes modelNameUpper

-- | anytime we're messing with an existing file, create a backup copy as a courtesy
--   to the users.  
createBackupCopy fp = copyRec fp (addExt fp)
 where
  addExt fp = addExtension fp "bak"
  copyRec fpSrc fpDes = do
    exists <- isFile fpDes
    if exists 
      then copyRec fpSrc (addExt fpDes) 
      else do
        copyFile fpSrc fpDes 
        print $ ("Backed up " ++ show fpSrc ++ " to location: " ++ show fpDes :: Text)

makeFields :: [Text] -> ErrT [FieldDesc]
makeFields l = mapM (\t -> checkRes (parse parseField t)) l
 where
  checkRes (Done _ r) = return r
  checkRes (Partial f) = checkRes $  f DT.empty 
  checkRes r = throwError $ "parse error: " ++ show r

parseField :: Parser FieldDesc
parseField = do
  -- require a lowercase field name
  fldName <- DT.cons <$> satisfy DC.isLower <*> Data.Attoparsec.Text.takeWhile isValidFieldChar
  string "::"
  (fldTypeName, nullable) <- parseNonNullable <|> parseNullable
  case textToFieldType fldTypeName of
    Left e -> error $ DT.unpack e
    Right fldTypeVal -> return $ FieldDesc  fldName fldTypeVal nullable
 where
  parseType = DT.cons <$> satisfy DC.isUpper <*> Data.Attoparsec.Text.takeWhile isValidTypeChar
  parseNonNullable = do
    fldTypeName <- parseType
    endOfInput
    return (fldTypeName, False)
  parseNullable = do
    fldTypeName <- parseType
    _ <- char '?'
    endOfInput
    return (fldTypeName, True)
  isValidTypeChar = isValidFieldChar
  isValidFieldChar c = any ($ c) [DC.isAlphaNum, (== '_')]

  
-- | _TODO whip up some parsec to parse fieldName::Int? stuff the record below
data FieldDesc = FieldDesc { fdName :: Text , fdType :: FieldType , fdNullable :: Bool } 
  deriving (Show)

data FieldType 
  = PersistReference Text
  | FtInt
  | FtDay
  | FtText
  | FtHtml
  | FtDouble
  deriving (Show)

typeForFieldType :: FieldType -> Text
typeForFieldType (PersistReference ref) = ref ++ "Id"
typeForFieldType f = drop 2 . DT.pack $ show f 

textToFieldType :: Text -> Either Text FieldType
textToFieldType "Int" = Right FtInt
textToFieldType "Double" = Right FtDouble
textToFieldType "Text" = Right FtText
textToFieldType "Html" = Right FtHtml
textToFieldType "Day" = Right FtDay
textToFieldType t | "Id" `DT.isSuffixOf` t = Right $ PersistReference $ take (length t - 2) t
                  | otherwise = Left $ "Bad Field Type:" ++ t

formFieldForFieldType FtInt    = "intField   "
formFieldForFieldType FtDay    = "dayField   "
formFieldForFieldType FtText   = "textField  "
formFieldForFieldType FtHtml   = "htmlField  "
formFieldForFieldType FtDouble = "doubleField"
formFieldForFieldType (PersistReference persRef) = 
  "(selectField (optionsPersistKey [] [] (toPathPiece . entityKey)))"

fieldForFieldDesc isFirst modelNameUpper fd = concat
  [ "  ", if isFirst then "<$>" else "<*>"
  , " ", if fdNullable fd then "aopt" else "areq"
  , " ", formFieldForFieldType (fdType fd)
  , " \"", fdName fd, "\""
  , " (", modelNameLower, fieldNameUpper, " <$> m", modelNameUpper, ")"
  ]
 where
  fieldNameUpper = DT.cons (DC.toUpper $ DT.head $ fdName fd) (DT.tail $ fdName fd)
  modelNameLower = DT.cons (DC.toLower $ DT.head modelNameUpper) (DT.tail modelNameUpper)

formText modelNameUpper [] = throwError "need at least one field!"
formText modelNameUpper (hdField:tlFields) = return $ DT.intercalate "\n" $ 
  [ concat [ modelNameLower, "Form :: Maybe ", modelNameUpper, " -> Form ", modelNameUpper]
  , concat [ modelNameLower, "Form m", modelNameUpper ," = renderDivs $ ", modelNameUpper]
  , fieldForFieldDesc True modelNameUpper hdField
  ] ++ map (fieldForFieldDesc False modelNameUpper) tlFields 
 where
  modelNameLower = DT.cons (DC.toLower $ DT.head modelNameUpper) (DT.tail modelNameUpper)


checkTypes t = do
  liftIO $ mapM_ (print . show) t
  return () 

checkModelName n = do
  -- _TODO change this to conduits for good yesodkarma
  let isTbl s | length s > 0 = not . DC.isSpace $ DT.head s
              | otherwise = False
  models <- fmap (map (DT.filter (not . DC.isSpace)) . filter isTbl . DT.lines) 
                 (liftIO $ readTextFile modelsFp)
  when (n `elem` models) . throwError $ "Model name: " ++ n ++ " already exists!"
  return ()

checkFileExists :: FilePath -> ErrT ()
checkFileExists fp = do
  exists <- lift $ isFile fp
  unless exists . throwError $ "Missing file: " ++ either id id (toText fp)
  return () 
 
instance Error Text where strMsg = DT.pack
