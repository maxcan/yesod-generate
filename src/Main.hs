{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | main entry point for the generators.
--
--   in the future, we may support multiple generators.  
--   we dont now
module Main where

import Control.Monad.Error hiding (mapM_)
import Control.Monad.Error.Class
import Foundation
import qualified Data.Text as DT 
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import qualified Data.Char as DC
import System.IO (putStrLn, print)
import Yesod.Util.CodeGen
import Filesystem.Path.CurrentOS (fromText, toText)
import Filesystem
import Filesystem.Path hiding (concat)
import System.Console.CmdArgs
import Prelude (String)


type ErrT = ErrorT Text IO
main :: IO ()
main = do
  command <- cmdArgsRun (cmdArgsMode $ modes [model, view] &= program "yesod-generate")
  print command
  res <- runErrorT $ main_ command
  print (show res :: Text)
  return ()

-- ** Types and Annotations for CmdArgs
data Generator
  = Model 
    { modelBootstrap :: Bool
    , modelRootDir :: String
    , modelName  :: String -- ^ Cmdargs doesn't support Data.Text apparently
    , modelTypes :: [String]
    }
  | View
    { viewBootstrap :: Bool }
  deriving (Show, Eq, Ord, Data, Typeable, Read) 

view :: Generator ; view = View
  { viewBootstrap  = def &= explicit &= name "b"  &= help "Use Bootstrap Style Forms"
  } &= help "Not yet supported"
model :: Generator ; model = Model
  { modelBootstrap = def &= explicit &= name "b" &= help "Use Bootstrap Style Forms"
  , modelRootDir =   "." &= explicit &= name "root" &= help "Root directory of project" &= opt ("." :: String)
  , modelName =      def &= typ "MODELNAME" &= argPos 0
  , modelTypes =     def &= typ "FIELDS" &= args
  } &= help  "Generate a Yesod Model and Views"

main_ :: Generator -> ErrT ()
main_ (View _) = throwError "view not supported"
main_ (Model True _ _ _) = throwError "bootstrap not supported"
main_ (Model _ _rootDir _modelName _fields) = do 
  let modelNameUpper = DT.pack _modelName 
      rootDir   = fromText $ DT.pack _rootDir
      fieldsRaw = map DT.pack _fields
      handlerFp = fromText $ "Handler/" ++ modelNameUpper ++ ".hs"
      routesFp = rootDir ++ "config/routes" 
      modelsFp = rootDir ++ "config/models" 
  -- sanity check that files exist
  checkFileExists routesFp
  checkFileExists modelsFp
  checkModelName modelsFp modelNameUpper
  fields <- makeFields fieldsRaw
  when (length fields < (1 :: Int)) $ throwError "Need at least one field!"
  -- check for cabal file:
  cabalFp <- do
    dirContents <- liftIO $ listDirectory rootDir
    case filter (flip hasExtension "cabal") dirContents of
      [f] -> return f
      []  -> throwError "no cabal file found"
      _   -> throwError "multiple cabal files found"
  -- check that there isn't already a handler file with this name:
  unless (DC.isUpper $ DT.head modelNameUpper) $ throwError "model name must be uppercase"
  liftIO (putStrLn "Checking handler directory" >> createDirectory True "Handler")
  handlerExists <- liftIO $ isFile handlerFp
  when handlerExists $ throwError "Handler File already exists, cannot continue"
  ------------------------------------------------------------------------
  --  FIRE THE MISSILES - This is where we pass the point of no return  -- 
  ------------------------------------------------------------------------
  addHandlerModuleToCabalFile cabalFp $ "Handler." ++ modelNameUpper
  addModelToModelsFile modelsFp modelNameUpper fields
  genHandlerFile handlerFp modelNameUpper fields
  addRoutes  routesFp modelNameUpper
  liftIO $ putStrLn "Finished Successfully!"
  
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
genHandlerFile fp modelNameUpper flds = do
  generatedForm <- formText modelNameUpper flds
  liftIO $ writeTextFile fp $(codegen "generic-handler")
 where
  tilde = "~"
  modelNameLower = 
    DT.cons (DC.toLower $ DT.head modelNameUpper) (DT.tail modelNameUpper)
  

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
        
genRoutes :: Text -> Text
genRoutes modelNameUpper = $(codegen "routes")
 where
  modelNameLower =
    DT.cons (DC.toLower $ DT.head modelNameUpper) (DT.tail modelNameUpper)

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
createBackupCopy :: FilePath -> IO ()
createBackupCopy fp = copyRec fp (addExt fp)
 where
  addExt fp_ = addExtension fp_ "bak"
  copyRec fpSrc fpDes = do
    exists <- isFile fpDes
    if exists 
      then copyRec fpSrc (addExt fpDes) 
      else do
        copyFile fpSrc fpDes 
        print $ ("Backed up " ++ show fpSrc ++ " to location: " ++ show fpDes :: Text)

makeFields :: [Text] -> ErrT [FieldDesc]
makeFields = mapM (checkRes . (parse parseField))
 where
  checkRes (Done _ r) = return r
  checkRes (Partial f) = checkRes $  f DT.empty 
  checkRes r = throwError $ "parse error: " ++ show r

parseField :: Parser FieldDesc
parseField = do
  -- require a lowercase field name
  fldName <- DT.cons <$> satisfy DC.isLower <*> Data.Attoparsec.Text.takeWhile isValidFieldChar
  _ <- string "::"
  (fldTypeName, nullable) <- parseNonNullable <|> parseNullable
  case textToFieldType fldTypeName of
    Left e -> error e
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
typeForFieldType f = drop (2 :: Int) . DT.pack $ show f 

textToFieldType :: Text -> Either Text FieldType
textToFieldType "Int" = Right FtInt
textToFieldType "Double" = Right FtDouble
textToFieldType "Text" = Right FtText
textToFieldType "Html" = Right FtHtml
textToFieldType "Day" = Right FtDay
textToFieldType t | "Id" `DT.isSuffixOf` t = Right $ PersistReference $ take (length t - 2 :: Int) t
                  | otherwise = Left $ "Bad Field Type:" ++ t

formFieldForFieldType :: FieldType -> Text
formFieldForFieldType FtInt    = "intField   "
formFieldForFieldType FtDay    = "dayField   "
formFieldForFieldType FtText   = "textField  "
formFieldForFieldType FtHtml   = "htmlField  "
formFieldForFieldType FtDouble = "doubleField"
formFieldForFieldType (PersistReference _) = 
  "(selectField (optionsPersistKey [] [] (toPathPiece . entityKey)))"

fieldForFieldDesc :: Bool -> Text -> FieldDesc -> Text
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

formText :: Text -> [FieldDesc] -> ErrT Text
formText _ [] = throwError "need at least one field!"
formText modelNameUpper (hdField:tlFields) = return $ DT.intercalate "\n" $ 
  [ concat [ modelNameLower, "Form :: Maybe ", modelNameUpper, " -> Form ", modelNameUpper]
  , concat [ modelNameLower, "Form m", modelNameUpper ," = renderDivs $ ", modelNameUpper]
  , fieldForFieldDesc True modelNameUpper hdField
  ] ++ map (fieldForFieldDesc False modelNameUpper) tlFields 
 where
  modelNameLower = DT.cons (DC.toLower $ DT.head modelNameUpper) (DT.tail modelNameUpper)

checkModelName :: FilePath -> Text -> ErrT ()
checkModelName modelsFp n = do
  -- _TODO change this to conduits for good yesodkarma
  let isTbl s | length s > (0 :: Int) = not . DC.isSpace $ DT.head s
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
