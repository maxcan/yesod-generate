{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | main entry point for the generators.
--
--   in the future, we may support multiple generators.  
--   we dont now
module Main where

import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Trans (liftIO)
import System.Directory
import System.Environment (getArgs)
import Foundation
import qualified Data.Text as DT 
import qualified Data.Char as DC
import System.IO hiding (FilePath)

fpRoutes = "config/routes" :: Text
fpModels = "config/models" :: Text

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
  -- check args
  args <- liftIO getArgs
  case args of
    "model" : modelName : types -> do
      checkModelName $ DT.pack modelName 
    m : _ -> throwError $ "No generator for: " ++ DT.pack m
    [] -> throwError $ "Need some arguments!  Toss me a freakin bone here"
  

checkModelName n = do
  -- _TODO change this to conduits for good yesodkarma
  let filterTables = filter (not . DC.isSpace . DT.head)
  models <- fmap (map (DT.filter (not . DC.isSpace)) . filterTables . DT.lines . DT.pack) 
                 (liftIO . readFile $ DT.unpack fpModels)
  when (n `elem` models) . throwError $ "Model name: " ++ n ++ " already exists!"
  return ()

checkFileExists :: Text -> ErrT ()
checkFileExists fp = do
  exists <- lift $ doesFileExist $ DT.unpack fp
  unless exists . throwError $ "Missing file: " ++ fp
  return () 
 
instance Error Text where strMsg = DT.pack
