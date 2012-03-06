{-# LANGUAGE NoImplicitPrelude #-}
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
import System.Directory
import System.Environment (getArgs)
import Foundation
import qualified Data.Text as DT 
-- import Text.ParserCombinators.Parsec
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import qualified Data.Char as DC
import System.IO (print)
import Yesod.Util.CodeGen
import Filesystem.Path.CurrentOS (toText)
import Filesystem
import Filesystem.Path

fpRoutes = "config/routes" :: FilePath
fpModels = "config/models" :: FilePath

type ErrT = ErrorT Text IO
main :: IO ()
main = do
  res <- runErrorT main_
  print $ show res
  return ()

main_ :: ErrT ()
main_ = do 
  -- sanity check that files exist
  checkFileExists fpRoutes
  checkFileExists fpModels
  -- check for cabal file:
  cabalFp <- do
    dirContents <- liftIO $ listDirectory "."
    case filter (flip hasExtension ".cabal") dirContents of
      [f] -> return f
      []  -> throwError "no cabal file found"
      _   -> throwError "multiple cabal files found"
  -- check that there isn't already a handler file with this name:
  -- check args
  args <- liftIO getArgs
  case args of
    "model" : _modelName : _fields -> do
      let modelNameUpper = DT.pack _modelName 
          modelNameLower = DT.cons (DC.toLower $ DT.head modelNameUpper) (DT.tail modelNameUpper)
          fields = map DT.pack _fields
      
      unless (DC.isUpper $ DT.head modelNameUpper) 
        $ throwError "model name must be uppercase"
      checkModelName modelNameUpper
      fields <- makeFields fields
      let handlerFp = "Handler." ++ modelNameUpper ++ ".hs"
      handlerExists <- liftIO $ doesFileExist $ DT.unpack handlerFp
      when handlerExists $ throwError "Handler File already exists, cannot continue"
      ------------------------------------------------------------------------
      --  FIRE THE MISSILES - This is where we pass the point of no return  -- 
      ------------------------------------------------------------------------
      addHandlerModuleToCabalFile cabalFp $ "Handler." ++ modelNameUpper
      undefined
    m : _ -> throwError $ "No generator for: " ++ DT.pack m
    [] -> throwError $ "Need some arguments!  Toss me a freakin bone here"
  
addHandlerModuleToCabalFile :: FilePath -> Text -> ErrT ()
addHandlerModuleToCabalFile fp moduleName = do
  allLines <- liftIO $ lines <$> readTextFile fp
  case break ("other-modules" `DT.isInfixOf`) allLines of
    ([],_) -> throwError "malformed cabal file"
    (_,[]) -> throwError "malformed cabal file"
    (prevLines, otherModulesLine:restOfLines) -> do
      liftIO $ createBackupCopy fp
      undefined


-- | anytime we're messing with an existing file, create a backup copy as a courtesy
--   to the users.  
createBackupCopy fp = copyRec fp (addExt fp)
 where
  addExt fp = addExtension fp "bak"
  copyRec fpSrc fpDes = do
    exists <- isFile fpDes
    if exists 
      then copyRec fpSrc (addExt fpDes)
      else readTextFile fpSrc >>= writeTextFile fpDes

makeFields :: [Text] -> ErrT [FieldDesc]
makeFields l = mapM (\t -> checkRes (parse parseField t)) l
 where
  checkRes (Done _ r) = return r
  checkRes _ = throwError "parse error"

parseField :: Parser FieldDesc
parseField = do
  -- require a lowercase field name
  skipSpace
  fldName <- DT.cons <$> satisfy DC.isLower <*> Data.Attoparsec.Text.takeWhile isValidFieldChar
  string "::"
  fldTypeName <- DT.cons <$> satisfy DC.isUpper <*> Data.Attoparsec.Text.takeWhile isValidTypeChar
  case textToFieldType fldTypeName of
    Left e -> error $ DT.unpack e
    Right fldTypeVal -> return $ FieldDesc  fldName fldTypeVal False
      
  
 where
  isValidTypeChar = isValidFieldChar
  isValidFieldChar c = any ($ c) [DC.isAlphaNum, (== '_')]

  
-- | _TODO whip up some parsec to parse fieldName::Int? stuff the record below
data FieldDesc = FieldDesc { fdName :: Text , fdType :: FieldType , fdNullable :: Bool } 

data FieldType 
  = PersistReference Text
  | FtInt
  | FtDay
  | FtUTCTime
  | FtText
  | FtHtml
  | FtDouble

textToFieldType :: Text -> Either Text FieldType
textToFieldType "Int" = Right FtInt
textToFieldType "Double" = Right FtDouble
textToFieldType "Text" = Right FtText
textToFieldType "Html" = Right FtHtml
textToFieldType "Day" = Right FtDay
textToFieldType "UTCTime" = Right FtUTCTime
textToFieldType t | "Id" `DT.isSuffixOf` t = Right $ PersistReference $ take (length t - 2) t
                  | otherwise = Left $ "Bad Field Type:" ++ t

checkTypes t = do
  liftIO $ mapM_ (print . show) t
  return () 

checkModelName n = do
  -- _TODO change this to conduits for good yesodkarma
  let filterTables = filter (not . DC.isSpace . DT.head)
  models <- fmap (map (DT.filter (not . DC.isSpace)) . filterTables . DT.lines) 
                 (liftIO $ readTextFile fpModels)
  when (n `elem` models) . throwError $ "Model name: " ++ n ++ " already exists!"
  return ()

checkFileExists :: FilePath -> ErrT ()
checkFileExists fp = do
  exists <- lift $ isFile fp
  unless exists . throwError $ "Missing file: " ++ either id id (toText fp)
  return () 
 
instance Error Text where strMsg = DT.pack
