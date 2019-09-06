{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module SingletonsTestSuiteUtils (
   compileAndDumpTest
 , compileAndDumpStdTest
 , testCompileAndDumpGroup
 , ghcOpts
 , cleanFiles
 ) where

import Build_singletons   ( ghcPath, ghcFlags, rootDir          )
import Control.DeepSeq    ( NFData(..)                          )
import Control.Exception  ( Exception, evaluate, throw          )
import Data.Foldable      ( asum )
import Data.List          ( intercalate                         )
import Data.Text          ( Text                                )
import Data.String        ( IsString(fromString)                )
import System.Exit        ( ExitCode(..)                        )
import System.FilePath    ( takeBaseName, pathSeparator         )
import System.IO          ( IOMode(..), hGetContents, openFile  )
import System.FilePath    ( (</>)                               )
import System.Process     ( CreateProcess(..), StdStream(..)
                          , createProcess, proc, waitForProcess
                          , callCommand                         )
import Test.Tasty         ( TestTree, testGroup                 )
import Test.Tasty.Golden.Advanced
                          ( goldenTest                          )
import qualified Data.ByteString as BS
import qualified Turtle

-- Some infractructure for handling external process errors
newtype ProcessException = ProcessException String
  deriving newtype (Eq, Ord, Show)
  deriving anyclass Exception

-- directory storing compile-and-run tests and golden files
goldenPath :: FilePath
goldenPath = rootDir </> "tests/compile-and-dump/"

ghcVersion :: String
ghcVersion = ".ghc88"

-- GHC options used when running the tests
ghcOpts :: [String]
ghcOpts = ghcFlags ++ [
    "-v0"
  , "-c"
  , "-ddump-splices"
  , "-dsuppress-uniques"
  , "-fforce-recomp"
  , "-fprint-explicit-kinds"
  , "-O0"
  , "-i" ++ goldenPath
  , "-XTemplateHaskell"
  , "-XDataKinds"
  , "-XKindSignatures"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "-XMultiParamTypeClasses"
  , "-XGADTs"
  , "-XFlexibleInstances"
  , "-XUndecidableInstances"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XPolyKinds"
  , "-XFlexibleContexts"
  , "-XIncoherentInstances"
  , "-XLambdaCase"
  , "-XUnboxedTuples"
  , "-XInstanceSigs"
  , "-XDefaultSignatures"
  , "-XCPP"
  , "-XStandaloneDeriving"
  , "-XTypeApplications"
  , "-XEmptyCase"
  , "-XNoStarIsType"
  ]

-- Compile a test using specified GHC options. Save output to file, normalize
-- and compare it with golden file. This function also builds golden file
-- from a template file. Putting it here is a bit of a hack but it's easy and it
-- works.
--
-- First parameter is a path to the test file relative to goldenPath directory
-- with no ".hs".
compileAndDumpTest :: FilePath -> [String] -> TestTree
compileAndDumpTest testName opts =
    goldenTest
      (takeBaseName testName)
      (return ())
      compileWithGHC
      cmp
      acceptNewOutput
  where
    testPath         = testName ++ ".hs"
    templateFilePath = goldenPath ++ testName ++ ghcVersion ++ ".template"
    goldenFilePath   = goldenPath ++ testName ++ ".golden"
    actualFilePath   = goldenPath ++ testName ++ ".actual"

    compileWithGHC :: IO ()
    compileWithGHC = do
      hActualFile <- openFile actualFilePath WriteMode
      (_, _, _, pid) <- createProcess (proc ghcPath (testPath : opts))
                                              { std_out = UseHandle hActualFile
                                              , std_err = UseHandle hActualFile
                                              , cwd     = Just goldenPath }
      _ <- waitForProcess pid      -- see Note [Ignore exit code]
      normalizeOutput actualFilePath -- see Note [Output normalization]
      buildGoldenFile templateFilePath goldenFilePath
      return ()

    -- See Note [Diff options]
    cmd = ["diff", "-w", "-B", goldenFilePath, actualFilePath]

    -- This is invoked when the test suite is run with the --accept flag.
    -- In addition to updating the golden file, we should also update the
    -- template file (which is what is actually checked into version control).
    acceptNewOutput _ = do
      actualContents <- BS.readFile actualFilePath
      BS.writeFile goldenFilePath   actualContents
      BS.writeFile templateFilePath actualContents

    -- This is largely cargo-culted from the internals of
    -- tasty-golden's goldenVsFileDiff.cmp function.
    cmp _ _ | null cmd = error "compileAndDumpTest: empty command line"
    cmp _ _ = do
      (_, Just sout, _, pid)
        <- createProcess (proc (head cmd) (tail cmd)) { std_out = CreatePipe }
      -- strictly read the whole output, so that the process can terminate
      out <- hGetContents sout
      evaluate . rnf $ out

      r <- waitForProcess pid
      return $ case r of
        ExitSuccess -> Nothing
        _ -> Just out

-- Compile-and-dump test using standard GHC options defined by the testsuite.
-- It takes two parameters: name of a file containing a test (no ".hs"
-- extension) and directory where the test is located (relative to
-- goldenPath). Test name and path are passed separately so that this function
-- can be used easily with testCompileAndDumpGroup.
compileAndDumpStdTest :: FilePath -> FilePath -> TestTree
compileAndDumpStdTest testName testPath =
    compileAndDumpTest (testPath ++ (pathSeparator : testName)) ghcOpts

-- A convenience function for defining a group of compile-and-dump tests stored
-- in the same subdirectory. It takes the name of subdirectory and list of
-- functions that given the name of subdirectory create a TestTree. Designed for
-- use with compileAndDumpStdTest.
testCompileAndDumpGroup :: FilePath -> [FilePath -> TestTree] -> TestTree
testCompileAndDumpGroup testDir tests =
    testGroup testDir $ map ($ testDir) tests

-- Note [Ignore exit code]
-- ~~~~~~~~~~~~~~~~~~~~~~~
---- It may happen that compilation of a source file fails. We could find out
-- whether that happened by inspecting the exit code of GHC process. But it
-- would be tricky to get a helpful message from the failing test: we would need
-- to display stderr which we just wrote into a file. Luckliy we don't have to
-- do that - we can ignore the problem here and let the test fail when the
-- actual file is compared with the golden file.

-- Note [Diff options]
-- ~~~~~~~~~~~~~~~~~~~
--
-- We use following diff options:
--  -w - Ignore all white space.
--  -B - Ignore changes whose lines are all blank.

-- Note [Output normalization]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Output file is normalized inplace. Line numbers generated in splices:
--
--   Foo:(40,3)-(42,4)
--   Foo.hs:7:3:
--   Equals_1235967303
--
-- are turned into:
--
--   Foo:(0,0)-(0,0)
--   Foo.hs:0:0:
--   Equals_0123456789
--
-- This allows to insert comments into test file without the need to modify the
-- golden file to adjust line numbers.
--

normalizeOutput :: FilePath -> IO ()
normalizeOutput file = Turtle.inplace pat (fromString file)
  where
    pat :: Turtle.Pattern Text
    pat = asum
      [ "(0,0)-(0,0)" <$ numPair <* "-" <* numPair
      , ":0:0:" <$ ":" <* d <* ":" <* d <* "-" <* d
      , ":0:0" <$ ":" <* d <* ":" <* d
      , fromString @Text . numPeriod <$> Turtle.lowerBounded 10 Turtle.digit
      , fromString @Text . ('%' <$) <$> Turtle.lowerBounded 10 punctSym
      -- Remove pretty-printed references to the singletons package
      -- (e.g., turn `singletons-2.4.1:Sing` into `Sing`) to make the output
      -- more stable.
      , "" <$ "singletons-" <* verNum <* ":"
      ]
    verNum = d `Turtle.sepBy` Turtle.char '.'
    numPair = () <$ "(" <* d <* "," <* d <* ")"
    punctSym = Turtle.oneOf "!#$%&*+./>"
    numPeriod = zipWith const (cycle "0123456789876543210")
    d = Turtle.some Turtle.digit

buildGoldenFile :: FilePath -> FilePath -> IO ()
buildGoldenFile templateFilePath goldenFilePath = do
  hGoldenFile <- openFile goldenFilePath WriteMode
  runProcessWithOpts (UseHandle hGoldenFile) "awk"
            [ "-f", goldenPath </> "buildGoldenFiles.awk"
            , templateFilePath
            ]

runProcessWithOpts :: StdStream -> String -> [String] -> IO ()
runProcessWithOpts stdout program opts = do
  (_, _, Just serr, pid) <-
      createProcess (proc "bash" ["-c", (intercalate " " (program : opts))])
                    { std_out = stdout
                    , std_err = CreatePipe }
  ecode <- waitForProcess pid
  case ecode of
    ExitSuccess   -> return ()
    ExitFailure _ -> do
       err <- hGetContents serr -- Text would be faster than String, but this is
                                -- a corner case so probably not worth it.
       throw $ ProcessException ("Error when running " ++ program ++ ":\n" ++ err)

cleanFiles :: IO ()
cleanFiles = callCommand $ "rm -f " ++ rootDir </> "tests/compile-and-dump/*/*.{hi,o}"
