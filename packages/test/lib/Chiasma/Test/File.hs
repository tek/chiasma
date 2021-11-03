module Chiasma.Test.File where

import System.Directory (canonicalizePath, createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))

testDir :: IO FilePath
testDir = canonicalizePath "test"

-- raises exception if cwd is not the package root so we don't damage anything
tempDirIO :: FilePath -> IO FilePath
tempDirIO path = do
  base <- testDir
  let dir = base </> "temp"
  removePathForcibly dir
  createDirectoryIfMissing False dir
  let absPath = dir </> path
  createDirectoryIfMissing True absPath
  pure absPath

tempDir :: MonadIO m => FilePath -> m FilePath
tempDir path =
  liftIO $ tempDirIO path

fixture :: MonadIO m => FilePath -> m FilePath
fixture path = do
  base <- liftIO $ testDir
  pure $ base </> "fixtures" </> path
