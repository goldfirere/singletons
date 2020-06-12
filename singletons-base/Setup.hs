{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Monad

import Data.List
import Data.String

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Text

import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
      generateBuildModule flags pkg lbi
      buildHook simpleUserHooks pkg lbi hooks flags
  , confHook = \(gpd, hbi) flags ->
      confHook simpleUserHooks (amendGPD gpd, hbi) flags
  , haddockHook = \pkg lbi hooks flags -> do
      generateBuildModule (haddockToBuildFlags flags) pkg lbi
      haddockHook simpleUserHooks pkg lbi hooks flags
  }

-- | Convert only flags used by 'generateBuildModule'.
haddockToBuildFlags :: HaddockFlags -> BuildFlags
haddockToBuildFlags f = emptyBuildFlags
    { buildVerbosity = haddockVerbosity f
    , buildDistPref  = haddockDistPref f
    }

generateBuildModule :: BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule flags pkg lbi = do
  rootDir <- getCurrentDirectory
  let verbosity = fromFlag (buildVerbosity flags)
      distPref  = fromFlag (buildDistPref flags)
      distPref' | isRelative distPref = rootDir </> distPref
                | otherwise           = distPref
      -- Package DBs
      dbStack = withPackageDB lbi ++ [ SpecificPackageDB $ distPref' </> "package.conf.inplace" ]
      dbFlags = "-hide-all-packages" : "-package-env=-" : packageDbArgsDb dbStack

      ghc = case lookupProgram ghcProgram (withPrograms lbi) of
              Just fp -> locationPath $ programLocation fp
              Nothing -> error "Can't find GHC path"
  withTestLBI pkg lbi $ \suite suitecfg -> when (testName suite == fromString testSuiteName) $ do
    let testAutogenDir = autogenComponentModulesDir lbi suitecfg
    createDirectoryIfMissingVerbose verbosity True testAutogenDir
    let buildSingletonsBaseFile = testAutogenDir </> buildSingletonsBaseModule <.> "hs"
    withLibLBI pkg lbi $ \_ libCLBI -> do
      let libDeps = map fst $ componentPackageDeps libCLBI
          pidx = case dependencyClosure (installedPkgs lbi) libDeps of
                   Left p  -> p
                   Right _ -> error "Broken dependency closure"
          libTransDeps = map installedUnitId $ allPackages pidx
          singletonsBaseUnitId = componentUnitId libCLBI
          deps = formatDeps (singletonsBaseUnitId:libTransDeps)
          allFlags = dbFlags ++ deps
      writeFile buildSingletonsBaseFile $ unlines
        [ "module Build_singletons_base where"
        , ""
        , "ghcPath :: FilePath"
        , "ghcPath = " ++ show ghc
        , ""
        , "ghcFlags :: [String]"
        , "ghcFlags = " ++ show allFlags
        , ""
        , "rootDir :: FilePath"
        , "rootDir = " ++ show rootDir
        ]
  where
    formatDeps = map formatOne
    formatOne installedPkgId = "-package-id=" ++ display installedPkgId

    -- GHC >= 7.6 uses the '-package-db' flag. See
    -- https://ghc.haskell.org/trac/ghc/ticket/5977.
    packageDbArgsDb :: [PackageDB] -> [String]
    -- special cases to make arguments prettier in common scenarios
    packageDbArgsDb dbstack = case dbstack of
      (GlobalPackageDB:UserPackageDB:dbs)
        | all isSpecific dbs              -> concatMap single dbs
      (GlobalPackageDB:dbs)
        | all isSpecific dbs              -> "-no-user-package-db"
                                           : concatMap single dbs
      dbs                                 -> "-clear-package-db"
                                           : concatMap single dbs
     where
       single (SpecificPackageDB db) = [ "-package-db=" ++ db ]
       single GlobalPackageDB        = [ "-global-package-db" ]
       single UserPackageDB          = [ "-user-package-db" ]
       isSpecific (SpecificPackageDB _) = True
       isSpecific _                     = False

buildSingletonsBaseModule :: FilePath
buildSingletonsBaseModule = "Build_singletons_base"

testSuiteName :: String
testSuiteName = "singletons-base-test-suite"

amendGPD :: GenericPackageDescription -> GenericPackageDescription
amendGPD gpd = gpd
    { condTestSuites = map f (condTestSuites gpd)
    }
  where
    f (name, condTree)
        | name == fromString testSuiteName = (name, condTree')
        | otherwise                        = (name, condTree)
      where
        -- I miss 'lens'
        testSuite = condTreeData condTree
        bi = testBuildInfo testSuite
        om = otherModules bi
        am = autogenModules bi

        -- Cons the module to both other-modules and autogen-modules.
        -- At the moment, cabal-spec-2.0 and cabal-spec-2.2 don't have
        -- "all autogen-modules are other-modules if they aren't exposed-modules"
        -- rule. Hopefully cabal-spec-3.0 will have.
        --
        -- Note: we `nub`, because it's unclear if that's ok to have duplicate
        -- modules in the lists.
        om' = nub $ mn : om
        am' = nub $ mn : am

        mn = fromString buildSingletonsBaseModule

        bi' = bi { otherModules = om', autogenModules = am' }
        testSuite' = testSuite { testBuildInfo = bi' }
        condTree' = condTree { condTreeData = testSuite' }
