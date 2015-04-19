{-# LANGUAGE CPP, DeriveDataTypeable #-}
module SingletonsTestSuiteUtils (
   compileAndDumpTest
 , compileAndDumpStdTest
 , testCompileAndDumpGroup
 , ghcOpts
 ) where

import Control.Exception  ( Exception, throw                    )
import Control.Monad      ( liftM                               )
import Data.List          ( intercalate, find, isPrefixOf       )
import Data.Typeable      ( Typeable                            )
import System.Exit        ( ExitCode(..)                        )
import System.FilePath    ( takeBaseName, pathSeparator         )
import System.IO          ( IOMode(..), hGetContents, openFile  )
import System.Process     ( CreateProcess(..), StdStream(..)
                          , createProcess, proc, waitForProcess
                          , readProcess                         )
import System.Directory   ( doesFileExist                       )
import Test.Tasty         ( TestTree, testGroup                 )
import Test.Tasty.Golden  ( goldenVsFileDiff                    )

#if __GLASGOW_HASKELL__ < 709
import Distribution.PackageDescription.Parse         ( readPackageDescription    )
import Distribution.PackageDescription.Configuration ( flattenPackageDescription )
import Distribution.PackageDescription               ( PackageDescription(..)    )
import Distribution.Verbosity                        ( silent                    )
import Data.Version                                  ( showVersion               )
#endif
import Distribution.Package                          ( PackageIdentifier(..)     )
import Distribution.Text                             ( simpleParse               )
import Data.Version                                  ( Version(..)               )
import System.IO.Unsafe                              ( unsafePerformIO           )

#include "../dist/build/autogen/cabal_macros.h"

-- Some infractructure for handling external process errors
data ProcessException = ProcessException String deriving (Typeable)

instance Exception ProcessException

instance Show ProcessException where
    show (ProcessException msg) = msg
-- GHC executable name (if on path) or full path
ghcPath :: FilePath
ghcPath = "ghc"

-- directory storing compile-and-run tests and golden files
goldenPath :: FilePath
goldenPath = "tests/compile-and-dump/"

-- path containing compiled *.hi files. Relative to goldenPath.
-- See Note [-package-name hack]
includePath :: FilePath
includePath = "../../dist/build"

ghcVersion :: String
#if __GLASGOW_HASKELL__ <  708
ghcVersion = error "testsuite requires GHC 7.8 or newer"
#else
-- these conditions are a bit nonsense right now, but they are here
-- to demonstrate how to add support for different GHC versions
#if __GLASGOW_HASKELL__ < 709
ghcVersion = ".ghc78"
#else
ghcVersion = ".ghc710"
#endif
#endif

-- The mtl package made an incompatible change between 2.1.3.1 and 2.2.1. Because
-- test files are compiled outside of the cabal infrastructure, we need to check
-- the mtl version and behave accordingly. Argh. The more general solution to this
-- is to use cabal_macros.h and then use the package specifications in dist/setup-config.
-- This also uses a cabal sandbox, if it is around.
extraOpts :: [String]
extraOpts = unsafePerformIO $ do
  (ghcPackageDbOpts, ghcPkgOpts) <- do
     sandboxed <- doesFileExist "cabal.sandbox.config"
     if sandboxed
     then do
       let prefix = "package-db: "
           opts_from_config config =
             case find (prefix `isPrefixOf`) $ lines config of
               Nothing -> ([], [])
               Just db_line -> let package_db = drop (length prefix) db_line in
                               ( [ "-no-user-package-db"
                                 , "-package-db " ++ package_db ]
                               , [ "--no-user-package-db"  -- ghc-pkg is slightly different!
                                 , "--package-db=" ++ package_db ] )
       opts_from_config `liftM` readFile "cabal.sandbox.config"
     else return ([], [])
  mtl_string <- readProcess "ghc-pkg" (ghcPkgOpts ++ ["latest", "mtl"]) ""
  let Just (PackageIdentifier { pkgVersion = ver }) = simpleParse mtl_string
      firstModernVersion = Version [2,2,1] []
      mtlOpt | ver >= firstModernVersion = ["-DMODERN_MTL"]
             | otherwise                 = []
  return $ ghcPackageDbOpts ++ mtlOpt

#if __GLASGOW_HASKELL__ < 709
-- the version number of "singletons"
singletonsVersion :: String
singletonsVersion = unsafePerformIO $ do
  gpd <- readPackageDescription silent "singletons.cabal"
  let pd = flattenPackageDescription gpd
  return $ showVersion $ pkgVersion $ package pd
#endif

-- GHC options used when running the tests
ghcOpts :: [String]
ghcOpts = extraOpts ++ [
    "-v0"
  , "-c"
#if __GLASGOW_HASKELL__ < 709
  , "-package-name singletons-" ++ singletonsVersion -- See Note [-package-name hack]
#else
  , "-this-package-key " ++ CURRENT_PACKAGE_KEY
#endif
  , "-ddump-splices"
  , "-dsuppress-uniques"
  , "-fforce-recomp"
  , "-fprint-explicit-kinds"
  , "-i" ++ includePath   -- necessary because some tests use these modules
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
  ]

-- Note [-package-name hack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We want to avoid installing singletons package before running the
-- testsuite, because in this way we prevent double compilation of the
-- library. To do this we pass -package-name option to GHC to convince
-- it that the test files are actually part of the current
-- package. This means that library doesn't have to be installed
-- globally and interface files generated during library compilation
-- can be used when compiling test cases. We use "-i" option to point
-- GHC to directory containing compiled interface files.

-- Compile a test using specified GHC options. Save output to file, filter with
-- sed and compare it with golden file. This function also builds golden file
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
      filterWithSed actualFilePath -- see Note [Normalization with sed]
      buildGoldenFile templateFilePath goldenFilePath
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
  , file
  ]

buildGoldenFile :: FilePath -> FilePath -> IO ()
buildGoldenFile templateFilePath goldenFilePath = do
  hGoldenFile <- openFile goldenFilePath WriteMode
  runProcessWithOpts (UseHandle hGoldenFile) "awk"
            [ "-f", "tests/compile-and-dump/buildGoldenFiles.awk"
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
