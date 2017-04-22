#!/usr/bin/env stack
-- stack --resolver lts-5.15 --install-ghc runghc --package turtle
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

{-| This script will automatically clone a given git repository at a specific
    commit into a temporary directory and parse the output of @cabal bench@
    into a CSV format readable by <https://github.com/nomeata/gipeda gipeda>.

    Currently, only the number of build warnings and standard criterion timing
    output are recognized.

    You can execute this script by running @stack cloben.hs@ in your project directory,
    as @stack cloben.hs repo commit@ if you don't have a local clone of you project or
    even as @./cloben.hs repo commit@ if marked as executable.
-}

module Main where

import           Control.Arrow             ((***))
import           Control.Exception         (bracket)
import qualified Control.Foldl             as Fold
import           Data.Char                 (isSpace)
import           Data.Either               (lefts)
import           Data.Text                 (Text, pack, unlines, unpack)
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem
import           Numeric                   (fromRat, showFFloat)
import           Prelude                   hiding (FilePath, unlines)
import           System.IO.Temp            (withSystemTempDirectory)
import           System.Process            (readProcessWithExitCode)
import           Turtle                    hiding (echo)

#if MIN_VERSION_turtle(1,3,0)
#else
lineToText :: Text -> Text
lineToText = id
#endif

echo :: (MonadIO io) => Text -> io ()
echo = printf (s % "\n")

{-| A gipeda metric, later to be displayed in a graph. The `Text` will be used as
    the name of the benchmark, the `Rational` is the actual metric which will be
    graphed. this will be the timing nanoseconds for benchmarks, or the number
    of build warnings.
-}
type Metric
  = (Text, Rational)


{-| Parses the command line and optionally creates a temporary directory into which to clone
    the passed repository (see @cloneRecursiveAndCheckout@).
    After that, @compileAndBenchmark@ returns the parsed metrics which are then converted
    into the gipeda CSV format.
-}
main :: IO ()
main = sh $ do
  (cloneOpts, verbose) <- options "cloben - optionally clone, benchmark and create gipeda logs" parser
  dir <- case cloneOpts of
    Just (repo, commit) -> do
      dir <- using (mksystempdir "cloben")
      cloneRecursiveAndCheckout repo commit dir verbose
      return dir
    Nothing -> pwd
  metrics <- compileAndBenchmark dir verbose
  echo (toCSV metrics)


parser :: Parser (Maybe (Text, Text), Bool)
parser = (,) <$> optional cloneOpts <*> verbose
  where
    cloneOpts = (,)
      <$> argText "repo" "URL or file path of the repository to clone"
      <*> argText "commit" "SHA prefix of the specific commit to benchmark"
    verbose =
      switch "verbose" 'v' "Output helpful debug messages as well as shell output"


-- | Like `Turtle.mktempdir`, but no need to specify a parent
mksystempdir :: Text -> Managed FilePath
mksystempdir prefix = do
  let
    prefix' =
      unpack prefix
  dir' <- managed (withSystemTempDirectory prefix')
  -- We need to cd back into home before we delete dir' again,
  -- otherwise we will quit with an error
  managed (bracket (return ()) (const (home >>= cd)))
  return (Filesystem.decodeString dir')


lefts' :: Fold (Either l r) [l]
lefts' =
  Fold.Fold (\f -> either (\x -> f . (x:)) (const id)) id ($ [])


{-| @cloneRecursiveAndCheckout repo commit dir verbose@ effectively performs a
    recursive @git clone@ on @repo@ and checks out the specified @commit@ into
    the directory given by @dir@.

    If @verbose@ is @True@, useful debug output
    is printed which normally interferes with the CSV output.
-}
cloneRecursiveAndCheckout :: Text -> Text -> FilePath -> Bool -> Shell ()
cloneRecursiveAndCheckout repo commit dir verbose = do
  let
    log text =
      when verbose (echo text)

  -- git seems to pipe to stderr mostly... So it won't pollute our audit
  log "> git clone <repo> <dir>"
  (clone, cloneOutput) <- procStrict "git" ["clone", "--quiet", repo, format fp dir] empty
  reportError "git clone --quiet <repo> <dir>" clone cloneOutput
  log "> Changing into the directory of the repository"
  cd dir
  log "> git reset --hard <commit>"
  (reset, resetOutput) <- procStrict "git" ["reset", "--hard", commit] empty
  reportError "git reset --hard <commit>" reset resetOutput
  shellAndReportError "git submodule update --init --recursive --quiet" log
  return ()


{-| @compileAndBenchmark projectDir verbose@ builds the cabal project at @projectDir@
    with enabled benchmarks. It will try to utilize stack if at all possible
    and will fall back to using cabal sandboxes.

    The number of warnings is extracted as a @Metric@ as @build/warnings;n@.

    After that, a @cabal bench@ is performed, of which the output is parsed for
    standard criterion timing output, where the timings are in nanoseconds.
    See `criterionBenchmarks`.

    If @verbose@ is @True@, useful debug output
    is printed which normally interferes with the CSV output.

    Also @GHC_PACKAGE_PATH@ is set when executing this through @stack@,
    which causes the build to error. That's why we unset.
    Passing `--ghc-no-package-path` to `stack runghc` would help, but then we
    can't specify the `turtle` package. So, unsetting is the only viable solution,
    I think.

    When executing this script through @stack@, it will automatically find and use
    the @cabal@ and @ghc@ binaries from the stack configuration. E.g., the result
    of executing this script through @stack@ might have different results and
    might even work when no @ghc@ or @cabal@ is on the path!

    For @cabal build@, we need to parse stderr for warnings.
-}
compileAndBenchmark :: FilePath -> Bool -> Shell [Metric]
compileAndBenchmark projectDir verbose = do
  let
    log text =
      when verbose (echo text)

    cabalBench :: Shell (Text, Text)
    cabalBench = do
      log "> Unsetting GHC_PACKAGE_PATH"
      unset "GHC_PACKAGE_PATH"
      shellAndReportError "cabal sandbox init" log
      shellAndReportError "cabal install -j --only-dependencies --enable-bench" log
      shellAndReportError "cabal configure --enable-benchmark" log
      log "> cabal bench"
      -- cabal outputs warnings on stderr and benchmark statistics on stdout
      stderr <- fold (inshellWithErr "cabal build" empty)
                (unlines .  map lineToText <$> lefts')
      stdout <- snd <$> shellAndReportError "cabal bench" log
      return (stderr, stdout)

    stackInit :: Shell Bool
    stackInit = do
      exists <- testfile (projectDir </> "stack.yaml")
      if not exists
        then do
          log "> No stack.yaml found"
          log "> stack init --solver"
          (code, stdout, stderr) <- liftIO $
            readProcessWithExitCode "stack" ["init", "--solver"] ""
          log (pack stderr)
          return (code == ExitSuccess)
        else do
          log "> Found stack.yaml"
          return True

    tryStackAndFallBackToCabal :: Shell (Text, Text)
    tryStackAndFallBackToCabal = do
      log "> Changing in to the directory of the project"
      cd projectDir
      canUseStack <- stackInit
      if canUseStack
        then do
          log "> stack bench"
          -- stack outputs both warnings and benchmark statistics on stderr
          export "STACK_LOCK" "true"
          let
            cmd :: IsString s => s
            cmd = "stack bench --force-dirty --install-ghc"
          (exitCode, stdout, stderr) <- liftIO $
            readProcessWithExitCode "stack" ["bench", "--force-dirty", "--install-ghc"] ""
          reportError cmd exitCode (pack stderr)
          return (pack stderr, pack stderr)
        else do
          log "Falling back to cabal"
          cabalBench

    -- using head here is safe, since there is always a match
    benchmarks :: Text -> [Metric]
    benchmarks =
      head . match criterionBenchmarks

    warnings :: Text -> Metric
    warnings =
      head . match buildWarnings

  (uncurry (:) . (warnings *** benchmarks))  <$> tryStackAndFallBackToCabal


shellAndReportError :: Text -> (Text -> Shell ()) -> Shell (ExitCode, Text)
shellAndReportError cmd log = do
  log ("> " <> cmd)
  (code, output) <- shellStrict cmd empty
  reportError cmd code output
  return (code, output)


reportError :: Text -> ExitCode -> Text -> Shell ()
reportError cmd code output =
  case code of
    ExitSuccess -> return ()
    ExitFailure n -> die (cmd <> " failed with exit code " <> repr n <>
      ". Output:\n" <> output)


buildWarnings :: Pattern Metric
buildWarnings =
  nameAndLength <$> (selfless chars *> many (warning <* selfless chars))
    where
      warning :: Pattern ()
      warning =
        char ':' >> decimal >> char ':' >> decimal >> text ": Warning:" >> return ()

      nameAndLength :: [a] -> (Text, Rational)
      nameAndLength xs =
        ("build/warnings", fromIntegral (length xs))


criterionBenchmarks :: Pattern [Metric]
criterionBenchmarks =
  selfless chars *> (mconcat <$> many benchmarkGroup)
    where
      benchmarkGroup :: Pattern [Metric]
      benchmarkGroup = do
        text "Benchmark "
        group <- word
        char ':'
        selfless chars
        benchmarks <- many (benchmark <* selfless chars1)
        return (map (\(n, t) -> (group <> "/" <> n, t)) benchmarks)

      benchmark :: Pattern Metric
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


toCSV :: [Metric] -> Text
toCSV =
  unlines . map (\(name, metric) -> name <> ";" <> showRat metric)
    where
      showRat :: Rational -> Text
      showRat num =
        -- The Nothing is for showing all digits. Terminates for our input
        pack (showFFloat Nothing (fromRat num) "")
