-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- There is a library in rules_haskell that does something similar
-- but since we also want support for locating runfiles in JARs
-- it is simpler to have all code located here.
module DA.Bazel.Runfiles
  ( create
  , rlocation
  , locateRunfiles
  , mainWorkspace
  ) where

import qualified Bazel.Runfiles
import System.Directory
import System.Environment
import System.FilePath

mainWorkspace :: String
mainWorkspace = "com_github_digital_asset_daml"

data Runfiles
  = Resources FilePath
  | BazelRunfiles Bazel.Runfiles.Runfiles

create :: IO Runfiles
create = do
  execPath <- getExecutablePath
  let jarResources = takeDirectory execPath </> "resources"
  hasJarResources <- doesDirectoryExist jarResources
  if hasJarResources
      then pure $! Resources jarResources
      else BazelRunfiles <$> Bazel.Runfiles.create

rlocation :: Runfiles -> FilePath -> IO FilePath
rlocation (Resources resources) fp = do
  -- If the current executable was packaged using @package_app@, then data
  -- files might appear right underneath the resources directory.
  -- See @bazel_tools/packaging/packaging.bzl@.
  let shortPath = resources </> takeFileName fp
  let fullPath = resources </> fp
  shortPathExists <- doesPathExist shortPath
  pure $! if shortPathExists then shortPath else fullPath
rlocation (BazelRunfiles runfiles) fp =
  pure $! Bazel.Runfiles.rlocation runfiles fp

locateRunfiles :: FilePath -> IO FilePath
locateRunfiles fp = do
  runfiles <- create
  rlocation runfiles fp
