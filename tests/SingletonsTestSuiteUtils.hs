{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SingletonsTestSuiteUtils (
   compileAndDumpTest
 , compileAndDumpStdTest
 , testCompileAndDumpGroup
 , cabalArgs
 , cleanFiles
 , RootDir(..)
 ) where

import Control.Exception  ( Exception, throw                    )
import Data.List          ( intercalate                         )
import System.Exit        ( ExitCode(..)                        )
import System.FilePath    ( (</>), pathSeparator, takeBaseName  )
import System.IO          ( IOMode(..), hGetContents, openFile  )
import System.Process     ( CreateProcess(..), StdStream(..)
                          , createProcess, proc, waitForProcess
                          , callCommand                         )
import Test.Tasty         ( TestTree, testGroup                 )
import Test.Tasty.Golden  ( goldenVsFileDiff                    )
import Test.Tasty.Options ( IsOption(..)                        )

{-
We augment the normal @tasty@ command-line options with one extra option:
rootdir, which allows a user to specify where the @singletons.cabal@ file
lives. One might think this would be unnecessary, since 90% of the time, a
user is going to run the tests from that very directory. But in a new-build
world, this assumption doesn't always hold true. In fact, it doesn't when
we test this very library on Travis, as we invoke the tests from a directory
/above/ the one that contains @singletons.cabal@!

Despite my best efforts, I (RGS) have not found a reliable way to automatically
detect where this directory lives, so in these 10% of cases, the simplest
solution is to allow the user to tell the test suite where this directory is
using a --rootdir flag. The code below accomplishes this.
-}
newtype RootDir = RootDir FilePath
  deriving stock Show
  deriving newtype (Eq, Ord)

instance IsOption RootDir where
  defaultValue = RootDir "."
  parseValue = Just . RootDir
  optionName = pure "rootdir"
  optionHelp = pure "Where singletons.cabal lives"

-- Some infractructure for handling external process errors
newtype ProcessException = ProcessException String
  deriving newtype (Eq, Ord, Show)
  deriving anyclass Exception


-- cabal executable name (if on path) or full path
cabalPath :: FilePath
cabalPath = "cabal"

-- The secret sauce to invoking ghc using a new-style store.
cabalArgs :: FilePath -> [String]
cabalArgs rootDir =
  [ "new-exec"
  , "ghc"
  , "--"
  ] ++ ghcOpts rootDir

-- directory storing compile-and-run tests and golden files
goldenPath :: FilePath -> FilePath
goldenPath rootDir = rootDir </> "tests/compile-and-dump/"

ghcVersion :: String
ghcVersion = ".ghc84"

-- GHC options used when running the tests
ghcOpts :: FilePath -> [String]
ghcOpts rootDir =
  [ "-v0"
  , "-c"
  , "-ddump-splices"
  , "-dsuppress-uniques"
  , "-fforce-recomp"
  , "-fprint-explicit-kinds"
  , "-O0"
  , "-i" ++ rootDir </> "tests/compile-and-dump"
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
  , "-XTypeInType"
  , "-XStandaloneDeriving"
  , "-XTypeApplications"
  , "-XEmptyCase"
  ]

-- Compile a test using specified GHC options. Save output to file, filter with
-- sed and compare it with golden file. This function also builds golden file
-- from a template file. Putting it here is a bit of a hack but it's easy and it
-- works.
--
-- First parameter is a path to the test file relative to goldenPath directory
-- with no ".hs".
compileAndDumpTest :: FilePath -> FilePath -> [String] -> TestTree
compileAndDumpTest rootDir testName opts =
    goldenVsFileDiff
      (takeBaseName testName)
      (\ref new -> ["diff", "-w", "-B", ref, new]) -- see Note [Diff options]
      goldenFilePath
      actualFilePath
      compileWithGHC
  where
    goldenDir        = goldenPath rootDir
    testPath         = testName ++ ".hs"
    templateFilePath = goldenDir ++ testName ++ ghcVersion ++ ".template"
    goldenFilePath   = goldenDir ++ testName ++ ".golden"
    actualFilePath   = goldenDir ++ testName ++ ".actual"

    compileWithGHC :: IO ()
    compileWithGHC = do
      hActualFile <- openFile actualFilePath WriteMode
      (_, _, _, pid) <- createProcess (proc cabalPath (opts ++ [testPath]))
                                              { std_out = UseHandle hActualFile
                                              , std_err = UseHandle hActualFile
                                              , cwd     = Just goldenDir }
      _ <- waitForProcess pid      -- see Note [Ignore exit code]
      filterWithSed actualFilePath -- see Note [Normalization with sed]
      buildGoldenFile rootDir templateFilePath goldenFilePath
      return ()

-- Compile-and-dump test using standard GHC options defined by the testsuite.
-- It takes two parameters: name of a file containing a test (no ".hs"
-- extension) and directory where the test is located (relative to
-- goldenPath). Test name and path are passed separately so that this function
-- can be used easily with testCompileAndDumpGroup.
compileAndDumpStdTest :: FilePath -> FilePath -> FilePath -> TestTree
compileAndDumpStdTest rootDir testName testPath =
    compileAndDumpTest rootDir (testPath ++ (pathSeparator : testName)) $
    cabalArgs rootDir

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

-- Note [Normalization with sed]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Output file is normalized with sed. Line numbers generated in splices:
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
-- Note that GNU sed (on Linux) and BSD sed (on MacOS) are slightly different.
-- We use conditional compilation to deal with this.

filterWithSed :: FilePath -> IO ()
filterWithSed file = runProcessWithOpts CreatePipe "sed"
#ifdef darwin_HOST_OS
  [ "-i", "''"
#else
  [ "-i"
#endif
  , "-e", "'s/([0-9]*,[0-9]*)-([0-9]*,[0-9]*)/(0,0)-(0,0)/g'"
  , "-e", "'s/:[0-9][0-9]*:[0-9][0-9]*/:0:0/g'"
  , "-e", "'s/:[0-9]*:[0-9]*-[0-9]*/:0:0:/g'"
  , "-e", "'s/[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/0123456789/g'"
  , "-e", "'s/[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/0123456789876543210/g'"
  , "-e", "'s/[!#$%&*+./>]\\{10\\}/%%%%%%%%%%/g'"
  , "-e", "'s/[!#$%&*+./>]\\{19\\}/%%%%%%%%%%%%%%%%%%%/g'"
  , file
  ]

buildGoldenFile :: FilePath -> FilePath -> FilePath -> IO ()
buildGoldenFile rootDir templateFilePath goldenFilePath = do
  hGoldenFile <- openFile goldenFilePath WriteMode
  runProcessWithOpts (UseHandle hGoldenFile) "awk"
            [ "-f", rootDir </> "tests/compile-and-dump/buildGoldenFiles.awk"
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

cleanFiles :: FilePath -> IO ()
cleanFiles rootDir = callCommand $ "rm -f " ++ rootDir </> "tests/compile-and-dump/*/*.{hi,o}"
