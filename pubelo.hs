#!/usr/bin/env stack
-- stack --resolver lts-3.20 --install-ghc runghc --package turtle 
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem
import System.IO.Temp (withSystemTempDirectory)
import Data.Text (unpack)


main :: IO ()
main = sh $ do
  (repo, commit) <- options "pubelo - pull, benchmark and create gipeda logs" parser
  dir <- using (mksystempdir "pubelo")
  cloneRecursiveAndCheckout repo commit dir
  --_ <- readline
  cabalBench dir


parser :: Parser (Text, Text)
parser =
  liftA2
    (,)
    (argText "repo" "URL or file path of the repository to pull from")
    (argText "commit" "SHA of the specific commit to benchmark")


mksystempdir :: Text -> Managed FilePath -- like mktempdir, but no need to specify a parent
mksystempdir prefix = do
  let prefix' = unpack prefix
  dir' <- managed (withSystemTempDirectory prefix')
  return (Filesystem.decodeString dir')


cloneRecursiveAndCheckout :: Text -> Text -> FilePath -> Shell ()
cloneRecursiveAndCheckout repo commit dir = do
  clone <- proc "git" ["clone", repo, format fp dir] empty
  reportError "git clone <repo> <dir>" clone
  echo ""
  echo "Changing into the directory of the repository"
  cd dir
  echo ""
  shellAndReportError "git reset --hard"
  echo ""
  shellAndReportError "git submodule update --init --recursive"
  echo ""


cabalBench :: FilePath -> Shell ()
cabalBench projectDir = do
  echo "Changing in to the directory of the project"
  cd projectDir
  echo "Unsetting GHC_PACKAGE_PATH"
  unset "GHC_PACKAGE_PATH"
  echo ""
  shellAndReportError "cabal sandbox init"
  echo ""
  shellAndReportError "cabal install -j --only-dependencies --enable-bench"
  echo ""
  shellAndReportError "cabal configure --enable-benchmark"
  echo ""
  shellAndReportError "cabal build"
  echo ""
  shellAndReportError "cabal bench"
  echo ""


shellAndReportError :: Text -> Shell ()
shellAndReportError cmd = do
  echo ("$ " <> cmd)
  code <- shell cmd empty
  reportError cmd code


reportError :: Text -> ExitCode -> Shell ()
reportError cmd code =
  case code of
    ExitSuccess -> return ()
    ExitFailure n -> die (cmd <> " failed with exit code " <> repr n)
