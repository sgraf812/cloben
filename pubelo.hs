#!/usr/bin/env stack
-- stack --resolver lts-3.20 --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath, unlines)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem
import System.IO.Temp (withSystemTempDirectory)
import Data.Char (isSpace)
import Data.Text (pack, unpack, unlines)
import Numeric (fromRat, showFFloat)


main :: IO ()
main = sh $ do
  (repo, commit, verbose) <- options "pubelo - pull, benchmark and create gipeda logs" parser
  dir <- using (mksystempdir "pubelo")
  cloneRecursiveAndCheckout repo commit dir verbose
  output <- cabalBench dir verbose

  let
    benchmarks :: [(Text, Nanoseconds)]
    benchmarks =
      case match criterionBenchmarks output of
        m : _ -> m
        [] -> []

  echo (toCSV benchmarks)


parser :: Parser (Text, Text, Bool)
parser =
  (,,)
    <$> argText "repo" "URL or file path of the repository to pull from"
    <*> argText "commit" "SHA of the specific commit to benchmark"
    <*> switch "verbose" 'v' "Output helpful debug messages as well as shell output"


mksystempdir :: Text -> Managed FilePath -- like mktempdir, but no need to specify a parent
mksystempdir prefix = do
  let prefix' = unpack prefix
  dir' <- managed (withSystemTempDirectory prefix')
  return (Filesystem.decodeString dir')


cloneRecursiveAndCheckout :: Text -> Text -> FilePath -> Bool -> Shell ()
cloneRecursiveAndCheckout repo commit dir verbose = do
  let
    log text =
      when verbose (echo text)

    quietFlag =
      if verbose then "--verbose" else "--quiet"

  log "> git clone <repo> <dir>"
  (clone, _) <- procStrict "git" ["clone", repo, format fp dir, quietFlag] empty
  reportError "git clone <repo> <dir>" clone
  log "> Changing into the directory of the repository"
  cd dir
  shellAndReportError "git reset --hard" log
  shellAndReportError "git submodule update --init --recursive" log
  return ()


cabalBench :: FilePath -> Bool -> Shell Text
cabalBench projectDir verbose = do
  let
    log text =
      when verbose (echo text)

  log "> Changing in to the directory of the project"
  cd projectDir
  log "> Unsetting GHC_PACKAGE_PATH"
  unset "GHC_PACKAGE_PATH"
  shellAndReportError "cabal sandbox init" log
  shellAndReportError "cabal install -j --only-dependencies --enable-bench" log
  shellAndReportError "cabal configure --enable-benchmark" log
  shellAndReportError "cabal build" log
  shellAndReportError "cabal bench" log


shellAndReportError :: Text -> (Text -> Shell ()) -> Shell Text
shellAndReportError cmd log = do
  log ("> " <> cmd)
  (code, output) <- shellStrict cmd empty
  log output
  reportError cmd code
  return output


reportError :: Text -> ExitCode -> Shell ()
reportError cmd code =
  case code of
    ExitSuccess -> return ()
    ExitFailure n -> die (cmd <> " failed with exit code " <> repr n)


type Nanoseconds
  = Rational


criterionBenchmarks :: Pattern [(Text, Nanoseconds)]
criterionBenchmarks =
  selfless chars *> (mconcat <$> (many benchmarkGroup))
    where
      benchmarkGroup :: Pattern [(Text, Nanoseconds)]
      benchmarkGroup = do
        text "Benchmark "
        group <- word
        char ':'
        selfless chars
        benchmarks <- many (benchmark <* selfless chars1)
        return (map (\(n, t) -> (group <> "/" <> n, t)) benchmarks)

      benchmark :: Pattern (Text, Nanoseconds)
      benchmark = do
        text "benchmarking "
        name <- word
        newline
        text "time"
        spaces1
        time <- timing
        spaces1
        siDivisor <- anySIDivisor
        return (name, time / siDivisor * 10^^9) -- we want nanoseconds

      word :: Pattern Text
      word =
        plus (satisfy (not . isSpace))

      decimalPlaces :: Rational -> Rational
      decimalPlaces n =
        if n < 1
          then n
          else decimalPlaces (n / 10)

      timing :: Pattern Rational
      timing = do
        integral <- decimal
        fraction <- (text "." *> decimal) <|> return 0
        return (integral + decimalPlaces (fromInteger fraction))

      powerOf10 :: Text -> Int -> Pattern Rational
      powerOf10 symbol e =
        text symbol >> return (10^^e)

      anySIDivisor :: Pattern Rational
      anySIDivisor =
        choice
          [ powerOf10 "s" 0
          , powerOf10 "ms" 3
          , powerOf10 "us" 6
          , powerOf10 "μs" 6 -- This is the unicode small greek letter mu
          , powerOf10 "μs" 6 -- This is the unicode micro sign, actually not used by criterion
          , powerOf10 "ns" 9
          , powerOf10 "ps" 12
          , powerOf10 "fs" 15
          , powerOf10 "as" 18
          ]

toCSV :: [(Text, Nanoseconds)] -> Text
toCSV =
  unlines . map (\(name, time) -> "benchmark/" <> name <> ";" <> formatNS time)
    where
      formatNS :: Nanoseconds -> Text
      formatNS num =
        -- The Nothing is for showing all digits. Terminates for our input
        pack ((showFFloat Nothing (fromRat num)) "")
