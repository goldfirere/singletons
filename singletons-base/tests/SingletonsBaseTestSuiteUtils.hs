{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module SingletonsBaseTestSuiteUtils (
   compileAndDumpTest
 , compileAndDumpStdTest
 , testCompileAndDumpGroup
 , ghcOpts
 , cleanFiles
 ) where

import Build_singletons_base ( ghcPath, ghcFlags, rootDir          )
import Control.Exception     ( Exception                           )
import Data.Foldable         ( asum                                )
import Data.Text             ( Text                                )
import Data.String           ( IsString(fromString)                )
import System.FilePath       ( takeBaseName, pathSeparator         )
import System.FilePath       ( (</>)                               )
import System.IO             ( IOMode(..), openFile                )
import System.Process        ( CreateProcess(..), StdStream(..)
                             , createProcess, proc, waitForProcess
                             , callCommand                         )
import Test.Tasty            ( TestTree, testGroup                 )
import Test.Tasty.Golden     ( goldenVsFileDiff                    )
import qualified Turtle

-- Some infractructure for handling external process errors
newtype ProcessException = ProcessException String
  deriving newtype (Eq, Ord, Show)
  deriving anyclass Exception

-- directory storing compile-and-run tests and golden files
goldenPath :: FilePath
goldenPath = rootDir </> "tests/compile-and-dump/"

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
  , "-XStandaloneKindSignatures"
  , "-XConstraintKinds"
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
    goldenVsFileDiff
      (takeBaseName testName)
      (\ref new -> ["diff", "-w", "-B", ref, new]) -- see Note [Diff options]
      goldenFilePath
      actualFilePath
      compileWithGHC
  where
    testPath         = testName ++ ".hs"
    goldenFilePath   = goldenPath ++ testName ++ ".golden"
    actualFilePath   = goldenPath ++ testName ++ ".actual"

    compileWithGHC :: IO ()
    compileWithGHC = do
      hActualFile <- openFile actualFilePath WriteMode
      (_, _, _, pid) <- createProcess (proc ghcPath (testPath : opts))
                                              { std_out = UseHandle hActualFile
                                              , std_err = UseHandle hActualFile
                                              , cwd     = Just goldenPath }
      _ <- waitForProcess pid        -- see Note [Ignore exit code]
      normalizeOutput actualFilePath -- see Note [Output normalization]
      return ()

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

{-
Note [Ignore exit code]
~~~~~~~~~~~~~~~~~~~~~~~
It may happen that the compilation of a source file fails. We could find out
whether that happened by inspecting the exit code of the `ghc` process. But it
would be tricky to get a helpful message from the failing test; we would need
to display the stderr that we just wrote into a file. Luckliy, we don't have to
do that - we can ignore the problem here and let the test fail when the
actual file is compared with the golden file.

Note [Diff options]
~~~~~~~~~~~~~~~~~~~
We use following diff options:
 -w - Ignore all white space.
 -B - Ignore changes whose lines are all blank.

Note [Output normalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Output file is normalized inplace. Line numbers generated in splices:

  Foo:(40,3)-(42,4)
  Foo.hs:7:3:
  Equals_1235967303

are turned into:

  Foo:(0,0)-(0,0)
  Foo.hs:0:0:
  Equals_0123456789

This allows inserting comments into test files without the need to modify the
golden file to adjust line numbers.
-}

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

cleanFiles :: IO ()
cleanFiles = callCommand $ "rm -f " ++ rootDir </> "tests/compile-and-dump/*/*.{hi,o}"
