{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text as Text
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
  , "-XGHC2024"
  , "-XTemplateHaskell"
  , "-XTypeFamilies"
  , "-XUndecidableInstances"
  , "-XIncoherentInstances"
  , "-XUnboxedTuples"
  , "-XDefaultSignatures"
  , "-XCPP"
  , "-XNoStarIsType"
  , "-XNoNamedWildCards"
  , "-XTypeAbstractions"
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

Note [Normalizing Windows path separators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
File paths are printed differently by GHC depending on which operating system
one uses:

* On Unix-like operating systems: Foo/Bar.hs
* On Windows:                     Foo\Bar.hs

This is annoying for golden testing, since it means that checking in output
that prints file paths will only work on some operating systems and not others.
To circumvent this problem, we normalize Windows-style path separators to
Unix-like ones.

One way to do this is to replace all occurrences of the '\' character with '/'.
This is a step too far, however, since this will normalize things like
(\x -> x) to (/x -> x). A reasonable middle ground is to require that the
characters before and after the '\' character are alphanumeric before
normalizing it. This just so happens to be the case for every path that is
checked into the singletons-base test suite. This approach isn't perfect, since
one could check in a golden file that prints a path with a non-alphanumeric
character. This seems very unlikely to happen, however, so we will only worry
about that issue should it ever arise in practice.

Moreover, this approach would normalize expressions like (id\x -> x)
to (id/x -> x), which could hypothetically happen with BlockArguments. However,
the vast majority of the expressions that we check into golden files arise from
-ddump-splices output, which puts surrounding whitespace after the backslashes
in lambda expressions. As a result, the expression above would be
pretty-printed as (id \ x -> x), which avoids the issue entirely. Again, we
will choose not to worry about this corner case unless it becomes an issue in
practice.
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
      , do x <- Turtle.alphaNum
           _ <- Turtle.char '\\'
           y <- Turtle.alphaNum
           pure $ Text.pack [x, '/', y]
      ]
    verNum = d `Turtle.sepBy` Turtle.char '.'
    numPair = () <$ "(" <* d <* "," <* d <* ")"
    punctSym = Turtle.oneOf "!#$%&*+./>"
    numPeriod = zipWith const (cycle "0123456789876543210")
    d = Turtle.some Turtle.digit

cleanFiles :: IO ()
cleanFiles = callCommand $ "rm -f " ++ rootDir </> "tests/compile-and-dump/*/*.{hi,o}"
