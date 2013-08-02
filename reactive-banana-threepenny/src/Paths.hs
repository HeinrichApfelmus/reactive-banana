{-# LANGUAGE CPP#-}
module Paths (getDataFile) where

import System.FilePath
import System.IO.Unsafe

#if buildExamples
-- using cabal
-- import System.Environment.Executable
-- import System.Info
import qualified Paths_reactive_banana_threepenny (getDataDir)

getDataDir :: IO FilePath
getDataDir = Paths_reactive_banana_threepenny.getDataDir

#else
-- using GHCi

getDataDir :: IO FilePath
getDataDir = return "../data/"

#endif

getDataFile x = fmap (</> x) getDataDir