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
import qualified Data.Char as DC
import System.IO (putStrLn, print)
import Text.Shakespeare.Text hiding (toText)
import Filesystem.Path.CurrentOS (fromText, toText)
import Filesystem
import Filesystem.Path hiding (concat)
import System.Console.CmdArgs
import Prelude (String)
import qualified Database.Persist.Quasi as DPQ
import Database.Persist.EntityDef (HaskellName (..), EntityDef (..), FieldDef (..), FieldType (..))

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
main_ (Model useBootstrap _rootDir _modelName _fields) = do 
  -- NOTE:
  --   if we add a Day to the model, need to add time to the cabal file and
  --   Data.Time.Calendar to model.hs
  --
  -- NEED TO ADD IMPORT TO Application.hs


  let modelNameUpper = DT.pack _modelName 
      rootDir   = fromText $ DT.pack _rootDir
      fieldsRaw = map DT.pack _fields
      spacer = "\n  " 
      persistText = modelNameUpper ++ spacer ++ 
        DT.intercalate " " ( map (DT.replace "," spacer) fieldsRaw)
      handlerFp = rootDir ++ "Handler" ++ addExtension (fromText modelNameUpper) "hs"
      routesFp = rootDir ++ "config/routes" 
      modelsFp = rootDir ++ "config/models" 
      appFp    = rootDir ++ "Application.hs"
  liftIO . print . DT.unpack $ "-----\nUsing persistText:\n" ++ persistText -- _DEBUG
  entityDef <- case DPQ.parse (DPQ.PersistSettings $ const "blankName") persistText of
    [l] -> return l
    []  -> throwError "Parse returned no entity defs"
    _   -> throwError "parse returned too many entity defs"
  -- sanity check that files exist
  checkFileExists routesFp
  checkFileExists modelsFp
  -- check fields:
  mapM_ persistFieldDefToFieldDesc $ entityFields entityDef -- will throw an erorr if there's a bad field
  checkModelName modelsFp modelNameUpper
  when (length (entityFields entityDef) < (1 :: Int)) $ throwError "Need at least one field!"
  when (modelNameUpper /= unHaskellName (entityHaskell entityDef)) 
    $ throwError "sanity check failed - model name NOT equal to parsed model name"
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
  addHandlerModuleToCabalFile cabalFp entityDef
  addModelToModelsFile modelsFp entityDef
  genHandlerFile useBootstrap handlerFp entityDef
  addLineToFile appFp 
                ("import Handler.Root" `DT.isInfixOf`) 
                ("import " ++ persistEntityDefToModule entityDef)
  addRoutes routesFp entityDef
  liftIO $ putStrLn "Finished Successfully!"
  
addModelToModelsFile :: FilePath -> EntityDef -> ErrT ()
addModelToModelsFile fp ed = do
  flds <- mapM persistFieldDefToFieldDesc $ entityFields ed
  liftIO $ do 
    createBackupCopy fp
    appendTextFile fp $ "\n" ++ DT.intercalate "\n" (modelLine : map fdToModel flds)
 where
  modelLine = unHaskellName (entityHaskell ed) ++ " json"
  fdToModel fd =
    "  " ++ fdName fd ++ " " ++ ftToType (fdType fd) ++ 
    if fdNullable fd then " Maybe" else ""

genHandlerFile :: Bool -> FilePath -> EntityDef -> ErrT ()
genHandlerFile useBootstrap fp ed  = do
  flds <- mapM persistFieldDefToFieldDesc $ entityFields ed
  case flds of
    [] -> throwError "Empty Field list.  genHandlerFile cannot handle this!"
    hdField:tlFields -> do 
      let generatedFormFields = "  " ++ DT.intercalate "\n  "
            ( (fieldForFieldDesc True modelNameUpper hdField)
            : (map (fieldForFieldDesc False modelNameUpper) tlFields ))
      liftIO $ writeTextFile fp $(codegenFile "codegen/generic-handler.cg")
 where
  renderFxn = if useBootstrap then "renderBootstrap" else "renderDivs" :: Text
  modelNameUpper = unHaskellName (entityHaskell ed)
  modelNameLower = decapitalize modelNameUpper

addUnqLineToFile 
  :: FilePath
  -> (Text -> Bool) -- ^ will add the line after the first line where this is true
  -> Text           -- ^ Text to import  
  -> ErrT ()
addUnqLineToFile fp whereToAdd whatToAdd = do
  allLines <- liftIO $ (map DT.strip . lines) <$> readTextFile fp
  unless (DT.strip whatToAdd `elem` allLines)
         (addLineToFile fp whereToAdd whatToAdd)

addLineToFile
  :: FilePath
  -> (Text -> Bool) -- ^ will add the line after the first line where this is true
  -> Text           -- ^ Text to import
  -> ErrT ()
addLineToFile fp whereToAdd whatToAdd = do
  liftIO $ print $ "Adding: " ++ whatToAdd ++ " to: " ++ either id id (toText fp)
  allLines <- liftIO $ lines <$> readTextFile fp
  case break whereToAdd allLines of
    ([],_) -> throwError "malformed file (probably missing other-modules field)"
    (_,[]) -> throwError "malformed file (probably missing other-modules field)"
    (prevLines, matchingLine:restOfLines) -> liftIO $ do
      createBackupCopy fp
      writeTextFile fp . DT.intercalate "\n" $  
        prevLines ++ (matchingLine : whatToAdd : restOfLines)

addHandlerModuleToCabalFile :: FilePath -> EntityDef -> ErrT ()
addHandlerModuleToCabalFile fp ed = do
  let moduleName = persistEntityDefToModule ed
      moduleLine = "                     " ++ moduleName
      dependsLines = [ "                 , unordered-containers          >= 0.1"
                     , "                 , aeson                         >= 0.6" ]
  addLineToFile fp ("other-modules" `DT.isInfixOf`) moduleLine
  mapM_ (addUnqLineToFile fp ("build-depends" `DT.isInfixOf`)) dependsLines
        
genRoutes :: EntityDef -> Text
genRoutes ed = $(codegenFile "codegen/routes.cg")
 where
  modelNameUpper = unHaskellName (entityHaskell ed)
  modelNameLower = decapitalize modelNameUpper

capitalize :: Text -> Text
capitalize t = DT.cons (DC.toUpper $ DT.head t) (DT.tail t)

decapitalize :: Text -> Text
decapitalize t = DT.cons (DC.toLower $ DT.head t) (DT.tail t)

addRoutes :: FilePath -> EntityDef -> ErrT ()
addRoutes fp ed = liftIO $ do
  createBackupCopy fp
  appendTextFile fp "\n"
  appendTextFile fp $ genRoutes ed

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

    -- parseField :: Parser FieldDesc
    -- parseField = do
    --   -- require a lowercase field name
    --   fldName <- DT.cons <$> satisfy DC.isLower <*> Data.Attoparsec.Text.takeWhile isValidFieldChar
    --   _ <- string "::"
    --   (fldTypeName, nullable) <- parseNonNullable <|> parseNullable
    --   case textToFieldType fldTypeName of
    --     Left e -> error e
    --     Right fldTypeVal -> return $ FieldDesc  fldName fldTypeVal nullable
    --  where
    --   parseType = DT.cons <$> satisfy DC.isUpper <*> Data.Attoparsec.Text.takeWhile isValidTypeChar
    --   parseNonNullable = do
    --     fldTypeName <- parseType
    --     endOfInput
    --     return (fldTypeName, False)
    --   parseNullable = do
    --     fldTypeName <- parseType
    --     _ <- char '?'
    --     endOfInput
    --     return (fldTypeName, True)
    --   isValidTypeChar = isValidFieldChar
    --   isValidFieldChar c = any ($ c) [DC.isAlphaNum, (== '_')]

-- ** Entity Def stuff
-- Sample Def:
-- [ EntityDef 
--   { entityHaskell = HaskellName {unHaskellName = "Person"}
--   , entityDB = DBName {unDBName = "Person"}
--   , entityID = DBName {unDBName = "id"}
--   , entityAttrs = []
--   , entityFields = 
--     [ FieldDef 
--       { fieldHaskell = HaskellName {unHaskellName = "age"}
--       , fieldDB = DBName {unDBName = "age"}
--       , fieldType = FTTypeCon Nothing "Int"
--       , fieldAttrs = []
--       }
--     , FieldDef 
--       { fieldHaskell = HaskellName {unHaskellName = "name"}
--       , fieldDB = DBName {unDBName = "name"}
--       , fieldType = FTTypeCon Nothing "Text"
--       , fieldAttrs = ["Maybe"]
--       }
--     ]
--   , entityUniques = []
--   , entityDerives = ["Show","Read","Eq"]
--   , entityExtra = fromList []}]
  
-- | _TODO whip up some parsec to parse fieldName::Int? stuff the record below
data FieldDesc = FieldDesc { fdName :: Text , fdType :: FdType , fdNullable :: Bool } 
  deriving (Show)

persistEntityDefToModule :: EntityDef -> Text
persistEntityDefToModule ed = "Handler." ++ unHaskellName (entityHaskell ed)

persistFieldDefToFieldDesc :: FieldDef -> ErrT FieldDesc
persistFieldDefToFieldDesc fd = do
  nullable <- case fieldAttrs fd of
                [] -> return False; ["Maybe"] -> return True
                l -> throwError $ DT.intercalate ": " ("bad attributes":l)
  tp <- case fieldType fd of
          FTTypeCon Nothing t -> textToFdType t
          t -> throwError $ "FieldType not supported: " ++ DT.pack (show t)
  return $ FieldDesc (unHaskellName $ fieldHaskell fd) tp nullable

data FdType 
  = PersistReference Text
  | FtInt
  | FtDay
  | FtText
  | FtHtml
  | FtDouble
  deriving (Show)

ftToType :: FdType -> Text
ftToType (PersistReference ref) = ref ++ "Id"
ftToType f = drop (2 :: Int) . DT.pack $ show f 

textToFdType :: Text -> ErrT FdType
textToFdType "Int" = return FtInt
textToFdType "Double" = return FtDouble
textToFdType "Text" = return FtText
textToFdType "String" = return FtText

-- textToFdType "Html" = return FtHtml
textToFdType "Day" = return FtDay
textToFdType t | "Id" `DT.isSuffixOf` t = return . PersistReference $ take (length t - 2 :: Int) t
                  | otherwise = throwError $ "Bad Field Type:" ++ t

formFieldForFdType :: FdType -> Text
formFieldForFdType FtInt    = "intField   "
formFieldForFdType FtDay    = "dayField   "
formFieldForFdType FtText   = "textField  "
formFieldForFdType FtHtml   = "htmlField  "
formFieldForFdType FtDouble = "doubleField"
formFieldForFdType (PersistReference _) = 
  "(selectField (optionsPersistKey [] [] (toPathPiece . entityKey)))"

fieldForFieldDesc :: Bool -> Text -> FieldDesc -> Text
fieldForFieldDesc isFirst modelNameUpper fd = concat
  [ "  ", if isFirst then "<$>" else "<*>"
  , " ", if fdNullable fd then "aopt" else "areq"
  , " ", formFieldForFdType (fdType fd)
  , " \"", fdName fd, "\""
  , " (", modelNameLower, fieldNameUpper, " <$> m", modelNameUpper, ")"
  ]
 where
  fieldNameUpper = capitalize $ fdName fd
  modelNameLower = decapitalize modelNameUpper

-- formText 
--   :: Bool -- ^ use bootstrap style forms
--   -> Text
--   -> [FieldDesc] 
--   -> ErrT Text
-- formText _ _ [] = throwError "need at least one field!"
-- formText useBootstrap modelNameUpper (hdField:tlFields) = return $ DT.intercalate "\n" $ 
--   [ concat [ modelNameLower, "Form :: Maybe ", modelNameUpper, " -> Form ", modelNameUpper]
--   , concat [ modelNameLower, "Form m", modelNameUpper ," = ", renderFxn ," $ ", modelNameUpper]
--   , fieldForFieldDesc True modelNameUpper hdField
--   ] ++ map (fieldForFieldDesc False modelNameUpper) tlFields 
--  where
--   modelNameLower = decapitalize modelNameUpper

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
