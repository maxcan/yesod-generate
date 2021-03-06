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

import           ClassyPrelude
import           ClassyPrelude.Classes
import           Control.Monad.Error hiding (mapM, mapM_, lift, liftIO)
import           Control.Monad.Error.Class
import           Data.Time.Clock (getCurrentTime)
import           Database.Persist.EntityDef
import           Distribution.VcsRevision.Git ( getRevision )
import           Filesystem
import           Filesystem.Path hiding (concat)
import           Filesystem.Path.CurrentOS (fromText, toText)
import           Language.Haskell.TH.Syntax (Exp (..), Lit (..), runIO)
import           Prelude (String)
import           System.Console.CmdArgs
import           Text.Shakespeare.Text hiding (toText)
import qualified Data.Char                   as DC
import qualified Data.List                   as DL 
import qualified Data.Maybe                  as DY
import qualified Data.Text                   as T
import qualified Database.Persist.Quasi as DPQ

concat = T.concat

type ErrT = ErrorT Text IO

main :: IO ()
main = do
  command <- cmdArgsRun (cmdArgsMode $ modes [model, dumpWidgets, versionGenerator]
                                             &= program "yesod-generate")
  print command
  res <- runErrorT $ main_ command
  print (show res :: Text)
  return ()

-- ** Types and Annotations for CmdArgs
data Generator
  = Model
    { modelJson :: Bool
    , modelRootDir :: String
    , modelName  :: String -- ^ Cmdargs doesn't support Data.Text apparently
    , modelTypes :: [String]
    }
  | DumpWidgets
    { dumpPath :: String
    , dumpRootDir :: String }
  | Widgets
    { viewJson :: Bool }
  | Version   
  deriving (Show, Eq, Ord, Data, Typeable)

versionGenerator :: Generator ; versionGenerator = Version &= help "output the git revno"
dumpWidgets :: Generator ; dumpWidgets = DumpWidgets
  { dumpPath       = "." &= explicit &= name "o" &= help "Where to dump the widgets to"
  , dumpRootDir  =   "." &= explicit &= name "root" &= help "Root directory of project"
  } &= help "Dump a widget file for an existing model"

model :: Generator ; model = Model
  { modelJson      = def &= explicit &= name "j" &= help "Create JSON Instances and Routes"
  , modelRootDir =   "." &= explicit &= name "root" &= help "Root directory of project" &= opt ("." :: String)
  , modelName =      def &= typ "MODELNAME" &= argPos 0
  , modelTypes =     def &= typ "FIELDS" &= args
  } &= help  "Generate a Yesod Model and Views"

main_ :: Generator -> ErrT ()
main_ (Widgets _) = throwError "view not supported"
main_ Version = liftIO $ putStrLn $ "Built against: " <> revno
 where
  revno :: Text; revno = pack $(do
    v <- runIO getRevision
    tm <- runIO getCurrentTime
    return $ LitE $ StringL $ case v of
          Nothing           -> "<no git repo> " <> show tm
          Just (hash,True)  -> pack hash <> " (with local modifications) " <> show tm
          Just (hash,False) -> pack hash <> " " <> show tm
    )

main_ (DumpWidgets _dumpPath _rootDir) = do
  let rootDir   = fromText $ T.pack _rootDir
      modelDefsFp = rootDir <> "config/models"
      widgetsFp   = rootDir <> fromText (T.pack _dumpPath)
      foundationFp = rootDir <> "Foundation.hs"
  when (_dumpPath == ".") $ throwError "need a path to output too"
  widgetsExists <- liftIO $ isFile widgetsFp
  if widgetsExists then liftIO $ createBackupCopy widgetsFp else genBareWidgetsHsFile widgetsFp
  foundationDatatype <- findFoundationDatatype foundationFp
  modelContent <- liftIO $ readTextFile modelDefsFp
  entityDefs <- map addContentTypeToEd <$> case DPQ.parse (DPQ.PersistSettings $ const "") modelContent of
    [] -> throwError "Parse returned no entity defs"
    l  -> return l
  -- (gitRev, dirty) <- $(runIO (maybe (error ("NO GIT REPO DATA AVAILABLE" :: Text)) return =<< getRevision))
  -- appendLineToFile widgetsFp . T.pack $ "  -- generated by yesod-generate " <> gitRev
  -- when dirty $ appendLineToFile widgetsFp "  -- BUILT ON A DIRTY REPO.  BE VERY CAREFUL.  Please report to i@cantor.mx"
  mapM_ (addWidgets widgetsFp foundationDatatype) entityDefs
  liftIO $ print ("Done! Note that you may need to add trasnformers to your build-depends if using yesod-generate for the first time" :: Text)

main_ (Model useJson _rootDir _modelName _fields) = do
  -- NOTE:
  --   if we add a Day to the model, need to add time to the cabal file and
  --   Data.Time.Calendar to model.hs
  --
  -- NEED TO ADD IMPORT TO Application.hs


  let modelNameUpper = T.pack _modelName
      rootDir   = fromText $ T.pack _rootDir
      fieldsRaw = map T.pack _fields
      spacer = "\n  "
      persistText = modelNameUpper <> spacer <>
        T.intercalate " " ( map (T.replace "," spacer) fieldsRaw)
      handlerFp = rootDir <> "Handler" <> addExtension (fromText modelNameUpper) "hs"
      routesFp  = rootDir <> "config/routes"
      widgetsFp = rootDir <> "Widgets.hs"
      foundationFp = rootDir <> "Foundation.hs"
      modelDefsFp = rootDir <> "config/models"
      modelModuleFp = rootDir <> "Model.hs"
      appFp    = rootDir <> "Application.hs"
  liftIO . print . T.unpack $ "-----\nUsing persistText:\n" <> persistText -- _DEBUG
  entityDef <- addContentTypeToEd <$> case DPQ.parse (DPQ.PersistSettings $ const "") persistText of
    [l] -> return l
    []  -> throwError "Parse returned no entity defs"
    _   -> throwError "parse returned too many entity defs"
  -- sanity check that files exist
  checkFileExists routesFp
  checkFileExists modelDefsFp
  writeTemplates <- chkTemplates  rootDir entityDef -- verify that we aren't overwriting any templates
  -- check fields:
  flds :: [FieldDesc] <- mapM persistFieldDefToFieldDesc $ entityFields entityDef -- will throw an erorr if there's a bad field
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
  unless (DC.isUpper $ T.head modelNameUpper) $ throwError "model name must be uppercase"
  liftIO (putStrLn "Checking handler directory" >> createDirectory True "Handler")
  handlerExists <- liftIO $ isFile handlerFp
  widgetsExists <- liftIO $ isFile widgetsFp
  foundationDatatype <- findFoundationDatatype foundationFp
  when handlerExists $ throwError "Handler File already exists, cannot continue"
  ------------------------------------------------------------------------
  --  FIRE THE MISSILES - This is where we pass the point of no return chk as much as possible above --
  ------------------------------------------------------------------------
  if widgetsExists then liftIO $ createBackupCopy widgetsFp else genBareWidgetsHsFile widgetsFp
  liftIO $ mapM_ createBackupCopy [routesFp, modelDefsFp, modelModuleFp, appFp, cabalFp]
  addRoutes routesFp entityDef
  when (FtDay `elem` map fdType flds) $ addDayStuff modelModuleFp cabalFp useJson
  when (FtImage `elem` map fdType flds) $ addImageStuff modelModuleFp routesFp entityDef useJson
  when (FtCountryCode `elem` map fdType flds) $ addCcStuff modelModuleFp cabalFp useJson
  addHandlerModuleToCabalFile cabalFp entityDef
  addDependsModule cabalFp "                 , yesod-json"
  addModelToModelsFile modelDefsFp modelModuleFp entityDef useJson
  genHandlerFile handlerFp entityDef useJson
  addWidgets     widgetsFp foundationDatatype entityDef
  writeTemplates
  addLineToFile appFp
                ("import Handler" `T.isInfixOf`)
                ("import " <> persistEntityDefToModule entityDef)
  liftIO $ putStrLn "Finished Successfully!"

addModelToModelsFile
  :: FilePath -- ^ path to config/models files
  -> FilePath -- ^ path to Model.hs which imports the above file
  -> EntityDef
  -> Bool -> ErrT ()
addModelToModelsFile defFp hsFp ed useJson = do
  flds :: [FieldDesc] <- mapM persistFieldDefToFieldDesc $ entityFields ed
  -- (gitRev, dirty) <- $(runIO (maybe (error ("NO GIT REPO DATA AVAILABLE" :: Text)) return =<< getRevision))
  -- appendLineToFile defFp . T.pack $ "  -- generated by yesod-generate " <> gitRev
  -- when dirty $ appendLineToFile defFp "  -- BUILT ON A DIRTY REPO.  BE VERY CAREFUL.  Please report to i@cantor.mx"
  appendLineToFile defFp $ T.intercalate "\n" (modelLine : map fdToModel flds) <> "\n"
  when useJson $ appendLineToFile hsFp $(codegenFile "codegen/model-to-json.cg")
  prependLineToFile hsFp flexibleInstances
  addImportToFile hsFp aeImport
  addImportToFile hsFp hmlImport
 where
  modelLine = if useJson then modelNameUpper <> " json" else modelNameUpper
  modelNameUpper = unHaskellName (entityHaskell ed)
  fdToModel fd =
    "  " <> fdName fd <> " " <> ftToType (fdType fd) <>
    if fdNullable fd then " Maybe" else ""

-- | if we pass the tests, this actually returns the function to call
chkTemplates :: FilePath -> EntityDef -> ErrT (ErrT ())
chkTemplates fp ed = do
  liftIO $ putStrLn "Checking template directory"
  liftIO $ createDirectory True genDirFp
  mapM_ chkTmplClean $ genTempates ed
  return . liftIO $ mapM_ writeTemplate (genTempates ed)
 where
  writeTemplate (n, t) =  writeTextFile (genFp n) t
  modelNameLower = decapitalize modelNameUpper
  modelNameUpper = unHaskellName (entityHaskell ed)
  genDirFp = fp <> "templates" <> fromText modelNameLower
  genFp s = genDirFp <> addExtension (fromText s) "hamlet"
  chkTmplClean :: (Text, Text)  -> ErrT (); chkTmplClean (s,_) = do
    exists <- liftIO . isFile $ genFp s
    when exists $ throwError ("Template File Exists: " <> modelNameLower <> "/" <> s)

findFoundationDatatype :: FilePath -> ErrT Text
findFoundationDatatype fp = do
  content <- liftIO $ readTextFile fp
  case filter ("mkYesod" `T.isPrefixOf`) . map T.strip $ T.lines content of
    [] -> throwError "Could not find mkYesod line in Foundation"
    [l] -> do
      let dt = T.takeWhile (/= '"') $ T.drop 1 $ T.dropWhile (/= '"') l
      when (length dt < (1 :: Int)) $ throwError "Found zero length foundation type"
      return dt
    _ -> throwError "Found too many potential datatypes"

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

genBareWidgetsHsFile :: FilePath -> ErrT ()
genBareWidgetsHsFile fp = do
  (liftIO $ isFile fp) >>= \b -> when b $ throwError "Widgets file exists, but the first check failed.  shouldn't happen"
  liftIO $ writeTextFile fp $(codegenFile "codegen/widgets.hs.cg")

for :: [a] -> (a -> b) -> [b]
for = flip map

addWidgets :: FilePath -> Text -> EntityDef -> ErrT ()
addWidgets fp appType ed = do
  flds <- mapM persistFieldDefToFieldDesc $ entityFields ed
  let
    dlSpacer = "\n      "
    thSpacer = "\n        "
    tdSpacer = thSpacer <> "  "
    modelFld fd = modelNameLower <> capitalize (fdName fd)
    curEntityVal = " (entityVal cur" <> modelNameUpper <> ")"
    curEntityKey = " (entityKey cur" <> modelNameUpper <> ")"
    tableHeaders = concat
      . ([thSpacer, "<th>"] <>)
      . for flds $ \fd -> concat [ thSpacer, "<th>", fdName fd]
    tableCells = concat
      . ([ tdSpacer, "<td>"
         , tdSpacer , "  <a href=@{",modelNameUpper,"DetailR ", curEntityKey, "}>"
         , tdSpacer , "    Details"
         ] <>)
      . for flds $ \fd -> concat $ [ tdSpacer, "<td>" ] <> case fdType fd of
      FtImage -> tdSpacer : if fdNullable fd
        then [ "  $maybe i <- ", modelFld fd, curEntityVal, tdSpacer
             , "    <img height=45px src=@{", capitalize (modelFld fd), "ImageR "
             , curEntityKey, "}>", tdSpacer
             , "  $nothing", tdSpacer
             , "    NULL VALUE"
             ]
        else [ "  <img height=45px src=@{", capitalize (modelFld fd), "ImageR ", curEntityKey, "}>" ]
      FtPersistReference refText -> tdSpacer : if fdNullable fd
        then [ "  $maybe i <- ", modelFld fd, curEntityVal, tdSpacer
             , "    <a href=@{", capitalize refText, "DetailR i}>", refText, ": #{toPathPiece i}", tdSpacer
             , "  $nothing", tdSpacer
             , "    NULL VALUE" ]
        else [ "  <a href=@{", capitalize refText, "DetailR (", modelFld fd,  curEntityVal, ")}>" , tdSpacer
             , "    ", refText, ": #{toPathPiece (", modelFld fd, curEntityVal, ")}"
             ]
      FtText -> tdSpacer : if fdNullable fd
        then [ "  $maybe i <- ", modelFld fd, curEntityVal, tdSpacer
             , "    #{i}", tdSpacer
             , "  $nothing", tdSpacer
             , "    NULL VALUE" ]
        else [ "  #{", modelFld fd, " (entityVal cur", modelNameUpper, ")}" ]
      _ -> [ "#{show (", modelFld fd, " (entityVal cur", modelNameUpper, "))}" ]
    toHtmlDlItems = concat $ map mkDl flds
    dlKyArg = if any ((== FtImage) . fdType) flds then "ky" else "_" :: Text
    mkDl fd = concat $ [ dlSpacer, "<dt>", fdName fd, dlSpacer, "<dd>"] <> case fdType fd of
      FtImage -> dlSpacer : if fdNullable fd
        then [ "  $maybe _ <- ", modelFld fd, " vl", dlSpacer
             , "    <img  src=@{", capitalize (modelFld fd), "ImageR ky}>"]
        else [dlSpacer, "  <img src=@{", capitalize (modelFld fd), "ImageR ky}>"]
      FtPersistReference refText -> dlSpacer : if fdNullable fd
        then [ "  $maybe i <- ", modelFld fd, " vl", dlSpacer
             , "    <a href=@{", capitalize refText, "DetailR i}>", refText, ": #{toPathPiece i}", dlSpacer
             , "  $nothing", dlSpacer
             , "    NULL VALUE" ]
        else [ dlSpacer, "  <a href=@{", capitalize refText, "DetailR "
             , "(", modelNameLower, capitalize (fdName fd), " vl)}>"
             , dlSpacer, "    ", refText, ": #{toPathPiece "
             , "(", modelNameLower, capitalize (fdName fd), " vl)}"
             ]
      FtText -> dlSpacer : if fdNullable fd
        then [ "  $maybe i <- ", modelFld fd, " vl", dlSpacer
             , "    #{i}", dlSpacer
             , "  $nothing", dlSpacer
             , "    NULL VALUE" ]
        else [ "  #{", modelNameLower, capitalize (fdName fd), " vl}" ]
      _ ->   [ "  #{show (", modelNameLower, capitalize (fdName fd), " vl)}"]
  appendLineToFile fp $(codegenFile "codegen/widget-snippet.cg")
 where
  modelNameUpper = unHaskellName (entityHaskell ed)
  modelNameLower = decapitalize modelNameUpper

genHandlerFile :: FilePath -> EntityDef -> Bool -> ErrT ()
genHandlerFile fp ed useJson  = do
  flds <- mapM persistFieldDefToFieldDesc $ entityFields ed
  let
    -- Generate the Form Fields.  Because of the Image stuff, this is a bit complicated
    boxFxn = (if includesImage then ("gen" <>) else id) modelNameUpper
    includesImage = FtImage `elem` map fdType flds
    imageImports =
      if includesImage then bslImport <> "\n" <> bsImport <> "\n" <> dsImport <> "\n" else "\n"
    formWhereDefs = if includesImage then wrappedBoxFxn else "\n"

    wrappingFoldFxn s (fd, _) | fdType fd == FtImageContentType = s
    wrappingFoldFxn (boxArgs, conArgs) (fd, elIdx) | fdNullable fd && (fdType fd == FtImage) =
      ( concat ["f", show elIdx, " ", boxArgs]
      , concat [ " (fmap (BS.concat . BSL.toChunks . fileContent) f", show elIdx,")"
               , " (fmap fileContentType f", show elIdx, ") ", conArgs] )
    wrappingFoldFxn (boxArgs, conArgs) (fd, elIdx) | (fdType fd == FtImage) =
      ( concat ["f", show elIdx, " ", boxArgs]
      , concat [ " (BS.concat . BSL.toChunks $ fileContent f", show elIdx,")"
               , " (fileContentType f", show elIdx, ") ", conArgs] )
    wrappingFoldFxn (boxArgs, conArgs) (_, elIdx) =
      ( concat ["f", show elIdx, " ", boxArgs], concat ["f", show elIdx, " ", conArgs])
    wrappedBoxFxn =
      let (b,c) = fold wrappingFoldFxn (T.empty, T.empty) . DL.reverse $ zip flds [(1 :: Int)..] in
      concat [" where\n  ", boxFxn, " ", b, " = ", modelNameUpper, " ", c]

    -- Generate the dt/dd entries for the toHtml instance
    jsonLayout :: Text = if useJson then "defaultLayoutJson" else "defaultLayout"
    jsonRep :: Text = if useJson then "RepHtmlJson" else "RepHtml" 
    jsonAllModels :: Text = if useJson then " all" <> modelNameUpper <> "s" else ""
    jsonModelDtl :: Text = if useJson 
        then "(Entity "  <> modelNameLower <> "Id " <> modelNameLower <> ")" 
        else ""
  case flds of
    [] -> throwError "Empty Field list.  genHandlerFile cannot handle this!"
    hdField:tlFields  -> do
    -- = DL.reverse l -- needed since we switched the code to a strict left fold
      generatedFormFields <- (("  " <>) . T.intercalate "\n  " . DY.catMaybes) <$> liftM2 (:)
        (fieldForFieldDesc True modelNameUpper hdField)
        (mapM (fieldForFieldDesc False modelNameUpper) tlFields )
      let imgHandlers = concat $ map mkHandler imgFlds
          imgFlds = filter ((== FtImage) . fdType) flds
          imgHandlerExports = case imgFlds of
            [] -> T.empty
            _  -> "\n  , " <> T.intercalate "\n  , " (map mkExport imgFlds)
          mkExport fd = "get" <> modelNameUpper <> capitalize (fdName fd) <> "ImageR"
          mkHandler fd | fdNullable fd =
            let fieldNameUpper = capitalize $ fdName fd in
            $(codegenFile "codegen/handler-image-nullable.cg")
                       | otherwise =
            let fieldNameUpper = capitalize $ fdName fd in
            $(codegenFile "codegen/handler-image.cg")
      liftIO $ writeTextFile fp ( $(codegenFile "codegen/generic-handler.cg") <> imgHandlers)
 where
  renderFxn = "renderBootstrap" :: Text
  modelNameUpper = unHaskellName (entityHaskell ed)
  modelNameLower = decapitalize modelNameUpper

appendLineAfterImports
  :: FilePath
  -> Text
  -> ErrT ()
appendLineAfterImports fp tx = do
  allLines <- liftIO $ T.lines <$> readTextFile fp
  let (hdrLines, (impLines, remLines)) = second (break (not . isImp)) $ break isImp allLines
      isImp = ("import" `T.isPrefixOf`)
  when (any (== []) [hdrLines, impLines, remLines]) $
    throwError $ "Parsing failed.  Could not add line: " <> tx
  unless (T.strip tx `elem` map T.strip allLines) $ liftIO $
    writeTextFile fp $ T.intercalate "\n" $ hdrLines <> impLines <> ["",tx, ""] <> remLines

prependLineToFile
  :: FilePath
  -> Text           -- ^ Text to import
  -> ErrT ()
prependLineToFile fp whatToAdd = do
  content <- liftIO $ readTextFile fp
  unless (T.strip whatToAdd `elem` map T.strip (T.lines content)) $ liftIO $ do
    print $ "Appending: " <> whatToAdd <> " to: " <> either id id (toText fp)
    writeTextFile fp $ whatToAdd <> "\n" <> content


appendLineToFile
  :: FilePath
  -> Text           -- ^ Text to import
  -> ErrT ()
appendLineToFile fp whatToAdd = do
  content <- liftIO $ readTextFile fp
  unless (T.strip whatToAdd `elem` map T.strip (T.lines content)) $ liftIO $ do
    print $ "Appending: " <> whatToAdd <> " to: " <> either id id (toText fp)
    writeTextFile fp $ content <> "\n" <> whatToAdd <> "\n"

addImportToFile :: FilePath -> Text -> ErrT ()
addImportToFile fp tx = addLineToFile fp ("import" `T.isPrefixOf`) tx

-- | Add a module to the other-modules section of the cabal file
addOtherModule :: FilePath -> Text -> ErrT ()
addOtherModule fp md = do
  let cond  = ("other-modules" `T.isInfixOf`)
  allLines <- liftIO $ T.lines <$> readTextFile fp
  if (or $ map cond allLines) 
    then addLineToFile fp cond ("                     " <> md)
    else addLineToFile fp ("library" `T.isPrefixOf`) ("    other-modules:   " <> md)

-- | Add a module dependency to the build-depends section of the cabal file.
addDependsModule :: FilePath -> Text -> ErrT ()
addDependsModule fp md = addLineToFile fp ("build-depends" `T.isInfixOf`) md

addLineToFile
  :: FilePath
  -> (Text -> Bool) -- ^ will add the line after the first line where this is true
  -> Text           -- ^ Text to import
  -> ErrT ()
addLineToFile fp whereToAdd whatToAdd = do
  allLines <- liftIO $ T.lines <$> readTextFile fp
  unless (T.strip whatToAdd `elem` map T.strip allLines) $
    case break whereToAdd allLines of
      (_,[]) ->
        throwError $ "Failed to add: <" <> whatToAdd <>"> to file: " <> show fp
      (prevLines, matchingLine:restOfLines) -> liftIO $ do
        print $ "Adding: " <> whatToAdd <> " to: " <> either id id (toText fp)
        writeTextFile fp . T.intercalate "\n" $
          prevLines <> (matchingLine : whatToAdd : restOfLines)

addHandlerModuleToCabalFile :: FilePath -> EntityDef -> ErrT ()
addHandlerModuleToCabalFile fp ed = do
  let moduleName = persistEntityDefToModule ed
      dependsLines = [ "                 , unordered-containers          >= 0.1"
                     , "                 , blaze-html                    >= 0.4.3.1"
                     , "                 , transformers                  >= 0.2.2"
                     , "                 , aeson                         >= 0.6" ] :: [Text]
  addOtherModule fp  moduleName
  addOtherModule fp  "Widgets"
  mapM_ (addDependsModule fp) dependsLines

genRoutes :: EntityDef -> Text
genRoutes ed = $(codegenFile "codegen/routes.cg")
 where
  modelNameUpper = unHaskellName (entityHaskell ed)
  modelNameLower = decapitalize modelNameUpper

capitalize :: Text -> Text
capitalize t = T.cons (DC.toUpper $ T.head t) (T.tail t)

decapitalize :: Text -> Text
decapitalize t = T.cons (DC.toLower $ T.head t) (T.tail t)

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
  flds :: [FieldDesc] <- filter (\f -> fdType f == FtImage) <$>
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
  addImportToFile modelHsFp dayImport
  when useJson $ do
    appendLineToFile modelHsFp "$(deriveJSON id ''Day)"
    addImportToFile modelHsFp aeThImport
  addDependsModule cabalFp "                 , time                          >= 1.4"

addRoutes :: FilePath -> EntityDef -> ErrT ()
addRoutes fp ed = appendLineToFile fp $ genRoutes ed

-- | anytime we're messing with an existing file, create a backup copy as a courtesy
--   to the users.
createBackupCopy :: FilePath -> IO ()
createBackupCopy fp = copyRec fp (0 :: Int)
 where
  copyRec fpSrc i = do
    let fpDes = addExtension fp (show i <> ".bak")
    exists <- isFile fpDes
    if exists
      then copyRec fpSrc (1 + i)
      else do
        copyFile fpSrc fpDes
        print $ ("Backed up " <> show fpSrc <> " to location: " <> show fpDes :: Text)

-- | _TODO whip up some parsec to parse fieldName::Int? stuff the record below
data FieldDesc = FieldDesc { fdName :: Text , fdType :: FdType , fdNullable :: Bool }
  deriving (Show)

addContentTypeToEd :: EntityDef -> EntityDef
addContentTypeToEd ed = ed { entityFields = fold foldFxn [] (entityFields ed) }
 where
  mkContentType fd = fd
    { fieldType = FTTypeCon Nothing "ImageContentType"
    , fieldHaskell = HaskellName ((unHaskellName $ fieldHaskell fd) <> "ContentType")
    , fieldDB = DBName ((unDBName $ fieldDB fd) <> "ContentType")
    }
  foldFxn s fd  | fieldType fd == FTTypeCon Nothing "Image" = s <> [fd , mkContentType fd]
                | otherwise = s <> [fd]


persistEntityDefToModule :: EntityDef -> Text
persistEntityDefToModule ed = "Handler." <> unHaskellName (entityHaskell ed)

persistFieldDefToFieldDesc :: FieldDef -> ErrT FieldDesc
persistFieldDefToFieldDesc fd = do
  nullable <- case fieldAttrs fd of
                [] -> return False; ["Maybe"] -> return True
                l -> throwError $ T.intercalate ": " ("bad attributes":l)
  tp <- case fieldType fd of
          FTTypeCon Nothing t -> textToFdType t
          t -> throwError $ "FieldType not supported: " <> T.pack (show t)
  return $ FieldDesc (unHaskellName $ fieldHaskell fd) tp nullable

data FdType
  = FtPersistReference Text
  | FtInt
  | FtBool
  | FtDay
  | FtText
  | FtCountryCode
  | FtHtml
  | FtDouble
  | FtImage
  | FtImageContentType
  deriving (Show, Eq, Ord)

ftToType :: FdType -> Text
ftToType (FtPersistReference ref) = ref <> "Id"
ftToType f = drop (2 :: Int) . T.pack $ show f

textToFdType :: Text -> ErrT FdType
textToFdType "Int" = return FtInt
textToFdType "Bool" = return FtBool
textToFdType "Double" = return FtDouble
textToFdType "Text" = return FtText
textToFdType "String" = return FtText
textToFdType "CountryCode" = return FtCountryCode
textToFdType "Image" = return FtImage
textToFdType "ImageContentType" = return FtImageContentType

-- textToFdType "Html" = return FtHtml
textToFdType "Day" = return FtDay
textToFdType t | "Id" `T.isSuffixOf` t = return . FtPersistReference $ take (length t - 2 :: Int) t
                  | otherwise = throwError $ "Bad Field Type:" <> t

formFieldForFdType :: FdType -> Text
formFieldForFdType FtInt          = "intField   "
formFieldForFdType FtBool         = "boolField  "
formFieldForFdType FtDay          = "dayField   "
formFieldForFdType FtText         = "textField  "
formFieldForFdType FtHtml         = "htmlField  "
formFieldForFdType FtCountryCode  = "(selectField optionsEnum)"
formFieldForFdType FtDouble       = "doubleField"
formFieldForFdType (FtPersistReference _) =
  "(selectField (optionsPersistKey [] [] (toPathPiece . entityKey)))"
formFieldForFdType f = error . unpack $ "cannot make a from field for : " <> (show f :: Text)

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
  let isTbl s | length s > (0 :: Int) = not . DC.isSpace $ T.head s
              | otherwise = False
  models <- fmap (map (T.filter (not . DC.isSpace)) . filter isTbl . T.lines) (liftIO $ readTextFile modelsFp)
  when (n `elem` models) . throwError $ "Model name: " <> n <> " already exists!"
  return ()

checkFileExists :: FilePath -> ErrT ()
checkFileExists fp = do
  exists <- lift $ isFile fp
  unless exists . throwError $ "Missing file: " <> either id id (toText fp)
  return ()

instance Error Text where strMsg = T.pack
