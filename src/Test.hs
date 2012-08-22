{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Control.Monad
import           Data.Monoid
import Shelly
import           Data.Time.Clock (getCurrentTime)
import qualified Data.Text                   as T 
import qualified Data.Text.Lazy              as TL 
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (threadDelay)
import           System.Environment
import           System.IO (hClose, hPutStrLn, hSetBuffering, BufferMode (..))
import           Filesystem.Path.CurrentOS (toText)
import           System.Process

default(TL.Text)
main = shelly . verbosely $ do
    homeDir <- pwd
    let workingDir      = "/tmp"
        appName         = "TestApp"
        distDir         = appDir </> "dist"
        hsEnvDistDir    = appDir </> "dist_src"
        appDir          = workingDir </> appName
        workingDirStr   = either (error "bad WD") T.unpack $ toText workingDir
        myName          = "Generate Test"
        dbType          = "s"  -- sqlite
        generateBin     = "yesod-generate"
        generatePath    = homeDir </> "dist" </> "build" </> generateBin </> generateBin
    echo "building yesod generate"
    cmd "cabal" "install"
    test_e (homeDir </> "dist") >>= \e -> unless e $ 
        cmd "ln" "-s" (homeDir </> "dist_src") (homeDir </> "dist")
    cd workingDir
    appExists <- test_e appDir
    when appExists $ rm_rf appDir 

    echo $ "working dir: " <> TL.pack (show workingDir)
    (Just yiStdin, _, _, procHdl) <- liftIO (createProcess ( shell "yesod init") {cwd = Just workingDirStr, std_out = Inherit , std_in = CreatePipe} )
    echo "about to send yesod commands"
    liftIO $ do
        hSetBuffering yiStdin LineBuffering
        hPutStrLn yiStdin $ T.unpack myName
        hPutStrLn yiStdin $ T.unpack appName
        hPutStrLn yiStdin $ T.unpack dbType
        hPutStrLn yiStdin ""
        hClose yiStdin
        waitForProcess procHdl
    
    
    cd appDir
    mkdir_p hsEnvDistDir
    cmd "ln"   "-s" hsEnvDistDir distDir
    cmd "cabal" "install"
    echo "about to run yesod generate"
    cmd generatePath "model" "-j" "Foo" "blah" "String," "boo" "Text," "dy" "Day," "im" "Image" "Maybe"
    cmd "cabal" "install"


    echo "built"

