{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | main entry point for the generators.
--
--   in the future, we may support multiple generators.  
--   we dont now.
module Main where

import Control.Monad.Error hiding (mapM_)
import Control.Monad.Error.Class
import Foundation
import Distribution.VcsRevision.Git ( getRevision )
import Language.Haskell.TH.Syntax (qRunIO)
import qualified Data.Text as DT 
import qualified Data.Char as DC
import qualified Data.Maybe as DY 
import System.IO (putStrLn, print)
import Text.Shakespeare.Text hiding (toText)
import Filesystem.Path.CurrentOS (fromText, toText)
import Filesystem
import Filesystem.Path hiding (concat)
import System.Console.CmdArgs
import Prelude (String)
import qualified Database.Persist.Quasi as DPQ
import Database.Persist.EntityDef 
import Control.Arrow (second)

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
    , modelJson :: Bool
    , modelRootDir :: String
    , modelName  :: String -- ^ Cmdargs doesn't support Data.Text apparently
    , modelTypes :: [String]
    }
  | View
    { viewBootstrap :: Bool, viewJson :: Bool }
  deriving (Show, Eq, Ord, Data, Typeable, Read) 

view :: Generator ; view = View
  { viewBootstrap  = def &= explicit &= name "b"  &= help "Use Bootstrap Style Forms"
  , viewJson       = def &= explicit &= name "j" &= help "Create JSON Instances and Routes"
  } &= help "Not yet supported" 
model :: Generator ; model = Model
  { modelBootstrap = def &= explicit &= name "b" &= help "Use Bootstrap Style Forms"
  , modelJson      = def &= explicit &= name "j" &= help "Create JSON Instances and Routes"
  , modelRootDir =   "." &= explicit &= name "root" &= help "Root directory of project" &= opt ("." :: String)
  , modelName =      def &= typ "MODELNAME" &= argPos 0
  , modelTypes =     def &= typ "FIELDS" &= args
  } &= help  "Generate a Yesod Model and Views"

main_ :: Generator -> ErrT ()
main_ (View _ _) = throwError "view not supported"
main_ (Model useBootstrap useJson _rootDir _modelName _fields) = do 
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
      modelDefsFp = rootDir ++ "config/models" 
      modelModuleFp = rootDir ++ "Model.hs"
      appFp    = rootDir ++ "Application.hs"
  liftIO . print . DT.unpack $ "-----\nUsing persistText:\n" ++ persistText -- _DEBUG
  entityDef <- addContentTypeToEd <$> case DPQ.parse (DPQ.PersistSettings $ const "") persistText of
    [l] -> return l
    []  -> throwError "Parse returned no entity defs"
    _   -> throwError "parse returned too many entity defs"
  -- sanity check that files exist
  checkFileExists routesFp
  checkFileExists modelDefsFp  
  writeTemplates <- chkTemplates  rootDir entityDef -- verify that we aren't overwriting any templates
  -- check fields:
  flds <- mapM persistFieldDefToFieldDesc $ entityFields entityDef -- will throw an erorr if there's a bad field
  checkModelName modelDefsFp modelNameUpper
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
  --  FIRE THE MISSILES - This is where we pass the point of no return chk as much as possible above -- 
  ------------------------------------------------------------------------
  liftIO $ mapM_ createBackupCopy [routesFp, modelDefsFp, modelModuleFp, appFp, cabalFp]
  addRoutes routesFp entityDef
  when (FtDay `elem` map fdType flds) $ addDayStuff modelModuleFp cabalFp useJson
  when (FtImage `elem` map fdType flds) $ addImageStuff modelModuleFp routesFp entityDef useJson
  when (FtCountryCode `elem` map fdType flds) $ addCcStuff modelModuleFp cabalFp useJson
  addHandlerModuleToCabalFile cabalFp entityDef
  addModelToModelsFile modelDefsFp modelModuleFp entityDef useJson
  genHandlerFile useBootstrap handlerFp entityDef
  writeTemplates 
  addLineToFile appFp 
                ("import Handler.Root" `DT.isInfixOf`) 
                ("import " ++ persistEntityDefToModule entityDef)
  liftIO $ putStrLn "Finished Successfully!"
  
addModelToModelsFile 
  :: FilePath -- ^ path to config/models files
  -> FilePath -- ^ path to Model.hs which imports the above file
  -> EntityDef 
  -> Bool -> ErrT ()
addModelToModelsFile defFp hsFp ed useJson = do
  flds <- mapM persistFieldDefToFieldDesc $ entityFields ed
  (gitRev, dirty) <- liftIO $ qRunIO (maybe (error ("NO GIT REPO DATA AVAILABLE" :: Text)) return =<< getRevision)
  appendLineToFile defFp . DT.pack $ "  -- generated by yesod-generate " ++ gitRev
  when dirty $ appendLineToFile defFp "  -- BUILT ON A DIRTY REPO.  BE VERY CAREFUL.  Please report to i@cantor.mx"
  appendLineToFile defFp $ DT.intercalate "\n" (modelLine : map fdToModel flds) ++ "\n"
  appendLineToFile hsFp $(codegenFile "codegen/model-to-json.cg")
  prependLineToFile hsFp flexibleInstances
  addImportToFile hsFp aeImport
  addImportToFile hsFp hmlImport
 where
  modelLine = if useJson then modelNameUpper ++ " json" else modelNameUpper
  modelNameUpper = unHaskellName (entityHaskell ed)
  fdToModel fd =
    "  " ++ fdName fd ++ " " ++ ftToType (fdType fd) ++ 
    if fdNullable fd then " Maybe" else ""

-- | if we pass the tests, this actually returns the function to call
chkTemplates :: FilePath -> EntityDef -> ErrT (ErrT ())
chkTemplates fp ed = do 
  liftIO $ putStrLn "Checking template directory" 
  liftIO $ createDirectory True genDirFp
  mapM_ chkTmplClean $ map fst $ genTempates ed
  return $ mapM_ writeTemplate $ genTempates ed
 where
  writeTemplate (n, t) = liftIO $ writeTextFile (genFp n) t
  modelNameLower = decapitalize modelNameUpper
  modelNameUpper = unHaskellName (entityHaskell ed)
  genDirFp = fp ++ "templates" ++ fromText modelNameLower
  genFp s = genDirFp ++ addExtension (fromText s) "hamlet"
  chkTmplClean s = do
    exists <- liftIO . isFile $ genFp s
    when exists $ throwError ("Template File Exists: " ++ modelNameLower ++ "/" ++ s)

genTempates :: EntityDef -> [ ( Text, Text) ]
genTempates ed = 
  [ ("edit" , $(codegenFile "codegen/templates/edit.hamlet.cg")) 
  , ("new"  , $(codegenFile "codegen/templates/new.hamlet.cg")) 
  , ("show" , $(codegenFile "codegen/templates/show.hamlet.cg")) 
  , ("index", $(codegenFile "codegen/templates/index.hamlet.cg")) 
  ]
 where
  modelNameLower = decapitalize modelNameUpper
  modelNameUpper = unHaskellName (entityHaskell ed)

genHandlerFile :: Bool -> FilePath -> EntityDef -> ErrT ()
genHandlerFile useBootstrap fp ed  = do
  flds <- mapM persistFieldDefToFieldDesc $ entityFields ed
  let 
    -- Generate the Form Fields.  Because of the Image stuff, this is a bit complicated
    boxFxn = (if includesImage then ("gen" ++) else id) modelNameUpper 
    includesImage = FtImage `elem` map fdType flds
    imageImports = 
      if includesImage then bslImport ++ "\n" ++ bsImport ++ "\n" ++ dsImport ++ "\n" else "\n"
    formWhereDefs = if includesImage then wrappedBoxFxn else "\n"
    wrappingFoldFxn (fd, _) s | fdType fd == FtImageContentType = s
    wrappingFoldFxn (fd, elIdx) (boxArgs, conArgs) | fdNullable fd && (fdType fd == FtImage) = 
      ( concat ["f", show elIdx, " ", boxArgs]
      , concat [ " (fmap (BS.concat . BSL.toChunks . fileContent) f", show elIdx,")"
               , " (fmap fileContentType f", show elIdx, ") ", conArgs] )
    wrappingFoldFxn (fd, elIdx) (boxArgs, conArgs) | (fdType fd == FtImage) = 
      ( concat ["f", show elIdx, " ", boxArgs]
      , concat [ " (BS.concat . BSL.toChunks $ fileContent f", show elIdx,")"
               , " (fileContentType f", show elIdx, ") ", conArgs] )
    wrappingFoldFxn (_, elIdx) (boxArgs, conArgs) = 
      ( concat ["f", show elIdx, " ", boxArgs], concat ["f", show elIdx, " ", conArgs])
    wrappedBoxFxn = 
      let (b,c) = foldr wrappingFoldFxn (DT.empty, DT.empty) $ zip flds [(1 :: Int)..] in 
      concat [" where\n  ", boxFxn, " ", b, " = ", modelNameUpper, " ", c]
    -- Generate the dt/dd entries for the toHtml instance
    dlSpacer = "\n      " 
    toHtmlDlItems = concat $ map mkDl flds
    mkDl fd | fdType fd == FtImage = concat
      [ dlSpacer, "<dt>", fdName fd
      , dlSpacer, "<dd>", dlSpacer, "  <img src=@{", modelNameUpper, capitalize (fdName fd), "ImageR ky}>"
      , "\n"]
    mkDl fd | otherwise = concat
      [ dlSpacer, "<dt>", fdName fd
      , dlSpacer, "<dd>#{show (", modelNameLower, capitalize (fdName fd), " vl)}"
      , "\n"]
  case flds of
    [] -> throwError "Empty Field list.  genHandlerFile cannot handle this!"
    hdField:tlFields -> do 
      generatedFormFields <- (("  " ++) . DT.intercalate "\n  " . DY.catMaybes) <$> liftM2 (:)
        (fieldForFieldDesc True modelNameUpper hdField)
        (mapM (fieldForFieldDesc False modelNameUpper) tlFields )
      let imgHandlers = concat $ map mkHandler imgFlds
          imgFlds = filter ((== FtImage) . fdType) flds
          imgHandlerExports = case imgFlds of
            [] -> DT.empty
            _  -> "\n  , " ++ DT.intercalate "\n  , " (map mkExport imgFlds)
          mkExport fd = "get" ++ modelNameUpper ++ capitalize (fdName fd) ++ "ImageR"
          mkHandler fd | fdNullable fd = 
            let fieldNameUpper = capitalize $ fdName fd in
            $(codegenFile "codegen/handler-image-nullable.cg")
                       | otherwise = 
            let fieldNameUpper = capitalize $ fdName fd in
            $(codegenFile "codegen/handler-image.cg")
      liftIO $ writeTextFile fp ( $(codegenFile "codegen/generic-handler.cg") ++ imgHandlers)
 where
  renderFxn = if useBootstrap then "renderBootstrap" else "renderDivs" :: Text
  modelNameUpper = unHaskellName (entityHaskell ed)
  modelNameLower = decapitalize modelNameUpper
 
appendLineAfterImports
  :: FilePath
  -> Text
  -> ErrT ()
appendLineAfterImports fp tx = do
  allLines <- liftIO $ lines <$> readTextFile fp
  let (hdrLines, (impLines, remLines)) = second (break (not . isImp)) $ break isImp allLines
      isImp = ("import" `DT.isPrefixOf`)
  when (any (== []) [hdrLines, impLines, remLines]) $
    throwError $ "Parsing failed.  Could not add line: " ++ tx
  unless (DT.strip tx `elem` map DT.strip allLines) $ liftIO $
    writeTextFile fp $ DT.intercalate "\n" $ hdrLines ++ impLines ++ ["",tx, ""] ++ remLines
  
prependLineToFile
  :: FilePath
  -> Text           -- ^ Text to import
  -> ErrT ()
prependLineToFile fp whatToAdd = do
  content <- liftIO $ readTextFile fp
  unless (DT.strip whatToAdd `elem` map DT.strip (lines content)) $ liftIO $ do
    print $ "Appending: " ++ whatToAdd ++ " to: " ++ either id id (toText fp)
    writeTextFile fp $ whatToAdd ++ "\n" ++ content

  
appendLineToFile
  :: FilePath
  -> Text           -- ^ Text to import
  -> ErrT ()
appendLineToFile fp whatToAdd = do
  content <- liftIO $ readTextFile fp
  unless (DT.strip whatToAdd `elem` map DT.strip (lines content)) $ liftIO $ do
    print $ "Appending: " ++ whatToAdd ++ " to: " ++ either id id (toText fp)
    writeTextFile fp $ content ++ "\n" ++ whatToAdd ++ "\n"

addImportToFile :: FilePath -> Text -> ErrT ()
addImportToFile fp tx = addLineToFile fp ("import" `DT.isPrefixOf`) tx

-- | Add a module to the other-modules section of the cabal file
addOtherModule :: FilePath -> Text -> ErrT ()
addOtherModule fp md = addLineToFile fp ("other-modules" `DT.isInfixOf`) md

-- | Add a module dependency to the build-depends section of the cabal file. 
addDependsModule :: FilePath -> Text -> ErrT ()
addDependsModule fp md = addLineToFile fp ("build-depends" `DT.isInfixOf`) md

addLineToFile
  :: FilePath
  -> (Text -> Bool) -- ^ will add the line after the first line where this is true
  -> Text           -- ^ Text to import
  -> ErrT ()
addLineToFile fp whereToAdd whatToAdd = do
  allLines <- liftIO $ lines <$> readTextFile fp
  unless (DT.strip whatToAdd `elem` map DT.strip allLines) $ 
    case break whereToAdd allLines of
      (_,[]) -> 
        throwError $ "malformed file: couldn't add: " ++ whatToAdd
      (prevLines, matchingLine:restOfLines) -> liftIO $ do
        print $ "Adding: " ++ whatToAdd ++ " to: " ++ either id id (toText fp)
        writeTextFile fp . DT.intercalate "\n" $  
          prevLines ++ (matchingLine : whatToAdd : restOfLines)

addHandlerModuleToCabalFile :: FilePath -> EntityDef -> ErrT ()
addHandlerModuleToCabalFile fp ed = do
  let moduleName = persistEntityDefToModule ed
      moduleLine = "                     " ++ moduleName
      dependsLines = [ "                 , unordered-containers          >= 0.1"
                     , "                 , blaze-html                    >= 0.4.3.1"
                     , "                 , aeson                         >= 0.6" ]
  addOtherModule fp moduleLine
  mapM_ (addDependsModule fp) dependsLines
        
genRoutes :: EntityDef -> Text
genRoutes ed = $(codegenFile "codegen/routes.cg")
 where
  modelNameUpper = unHaskellName (entityHaskell ed)
  modelNameLower = decapitalize modelNameUpper

capitalize :: Text -> Text
capitalize t = DT.cons (DC.toUpper $ DT.head t) (DT.tail t)

decapitalize :: Text -> Text
decapitalize t = DT.cons (DC.toLower $ DT.head t) (DT.tail t)

-- ** Imports
ccImport :: Text;   ccImport   = "import Data.ISO3166_CountryCodes (CountryCode (..))"
aeThImport :: Text; aeThImport = "import Data.Aeson.TH (deriveJSON)"
aeImport :: Text;   aeImport   = "import Data.Aeson"
dayImport :: Text;  dayImport  = "import Data.Time.Calendar (Day)"
hmlImport :: Text;  hmlImport  = "import qualified Data.HashMap.Lazy as HML"
bslImport :: Text;  bslImport  = "import qualified Data.ByteString.Lazy as BSL"
bsImport :: Text;   bsImport   = "import qualified Data.ByteString as BS"
dsImport :: Text;   dsImport   = "import Data.String (fromString)"
flexibleInstances :: Text
flexibleInstances   = "{-# LANGUAGE FlexibleInstances #-}"

imageTypeSyn :: Text ; imageTypeSyn = "type Image = BS.ByteString"
imageCtTypeSyn :: Text ; imageCtTypeSyn = "type ImageContentType = Text"

-- | Add the Country Code Imports
addCcStuff 
  :: FilePath -- ^ model fp
  -> FilePath -- ^ cabal fp
  -> Bool     -- ^ use JSON
  -> ErrT ()
addCcStuff modelHsFp cabalFp useJson = do
  when useJson $ appendLineToFile modelHsFp "$(deriveJSON id ''CountryCode)"
  appendLineToFile modelHsFp "derivePersistField \"CountryCode\""
  addImportToFile modelHsFp ccImport
  addDependsModule cabalFp "                 , iso3166-country-codes         >= 0.2"

addImageStuff 
  :: FilePath -- ^ model fp
  -> FilePath -- ^ routes fp
  -> EntityDef
  -> Bool     -- ^ True for Using JSON too
  -> ErrT ()
addImageStuff modelHsFp routesFp ed useJson = do
  addImportToFile modelHsFp bsImport
  appendLineAfterImports modelHsFp imageTypeSyn
  appendLineAfterImports modelHsFp imageCtTypeSyn
  flds <- filter (\f -> fdType f == FtImage) <$> 
          mapM persistFieldDefToFieldDesc (entityFields ed) -- will throw an erorr if there's a bad field
  let appFxn fd = 
        let fieldNameUpper = capitalize (fdName fd)
            fieldNameLower = fdName fd in
        appendLineToFile routesFp $(codegenFile "codegen/routes-image.cg")
  mapM_ appFxn flds
 where
  modelNameUpper = unHaskellName (entityHaskell ed)
  modelNameLower = decapitalize modelNameUpper

addDayStuff 
  :: FilePath -- ^ model fp
  -> FilePath -- ^ cabal fp
  -> Bool     -- ^ True for Using JSON too
  -> ErrT ()
addDayStuff modelHsFp cabalFp useJson = do
  when useJson $ do
    appendLineToFile modelHsFp "$(deriveJSON id ''Day)"
    addImportToFile modelHsFp aeThImport
    addImportToFile modelHsFp dayImport
  addDependsModule cabalFp "                 , time                          >= 1.4"

addRoutes :: FilePath -> EntityDef -> ErrT ()
addRoutes fp ed = appendLineToFile fp $ genRoutes ed

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

-- | _TODO whip up some parsec to parse fieldName::Int? stuff the record below
data FieldDesc = FieldDesc { fdName :: Text , fdType :: FdType , fdNullable :: Bool } 
  deriving (Show)

addContentTypeToEd :: EntityDef -> EntityDef
addContentTypeToEd ed = ed { entityFields = foldr foldFxn [] $ entityFields ed  }
 where
  mkContentType fd = fd 
    { fieldType = FTTypeCon Nothing "ImageContentType"
    , fieldHaskell = HaskellName ((unHaskellName $ fieldHaskell fd) ++ "ContentType")
    , fieldDB = DBName ((unDBName $ fieldDB fd) ++ "ContentType")
    }
  foldFxn fd s  | fieldType fd == FTTypeCon Nothing "Image" = fd : mkContentType fd : s 
                | otherwise = fd : s
  
  
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
  | FtCountryCode
  | FtHtml
  | FtDouble
  | FtImage
  | FtImageContentType
  deriving (Show, Eq, Ord, Read)

ftToType :: FdType -> Text
ftToType (PersistReference ref) = ref ++ "Id"
ftToType f = drop (2 :: Int) . DT.pack $ show f 

textToFdType :: Text -> ErrT FdType
textToFdType "Int" = return FtInt
textToFdType "Double" = return FtDouble
textToFdType "Text" = return FtText
textToFdType "String" = return FtText
textToFdType "CountryCode" = return FtCountryCode
textToFdType "Image" = return FtImage
textToFdType "ImageContentType" = return FtImageContentType

-- textToFdType "Html" = return FtHtml
textToFdType "Day" = return FtDay
textToFdType t | "Id" `DT.isSuffixOf` t = return . PersistReference $ take (length t - 2 :: Int) t
                  | otherwise = throwError $ "Bad Field Type:" ++ t

formFieldForFdType :: FdType -> Text
formFieldForFdType FtInt          = "intField   "
formFieldForFdType FtDay          = "dayField   "
formFieldForFdType FtText         = "textField  "
formFieldForFdType FtHtml         = "htmlField  "
formFieldForFdType FtCountryCode  = "(selectField optionsEnum)"
formFieldForFdType FtDouble       = "doubleField"
formFieldForFdType (PersistReference _) = 
  "(selectField (optionsPersistKey [] [] (toPathPiece . entityKey)))"
formFieldForFdType f = error $ "cannot make a from field for : " ++ (show f :: Text)

fieldForFieldDesc :: Bool -> Text -> FieldDesc -> ErrT (Maybe Text)
fieldForFieldDesc False _ fd | fdType fd == FtImageContentType = return Nothing
fieldForFieldDesc True _ fd  | fdType fd == FtImageContentType = 
  throwError "Found ImageContentType as first field.  We can't handle this.  Dying"
fieldForFieldDesc isFirst _ fd  | fdType fd == FtImage = return . Just $ concat
  [ "  ", if isFirst then "<$>" else "<*>"
  , " ", if fdNullable fd then "fileAFormOpt" else "fileAFormReq"
  , " \"", fdName fd, "\""
  -- , " Nothing"
  ]
fieldForFieldDesc isFirst modelNameUpper fd = return . Just $ concat
  [ "  ", if isFirst then "<$>" else "<*>"
  , " ", if fdNullable fd then "aopt" else "areq"
  , " ", formFieldForFdType (fdType fd)
  , " \"", fdName fd, "\""
  , " (", modelNameLower, fieldNameUpper, " <$> m", modelNameUpper, ")"
  ]
 where
  fieldNameUpper = capitalize $ fdName fd
  modelNameLower = decapitalize modelNameUpper

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
