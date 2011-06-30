-- Helper script to shadow that automatically generated by cabal, but pointing
-- to our local development directory.
--

module Paths_accelerate_cuda where

import System.Directory
import System.FilePath

getDataDir :: IO FilePath
getDataDir = (</> "accelerate-cuda") `fmap` getCurrentDirectory

