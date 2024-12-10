-- | A @cabal@ code generator used in the test suite for the @singletons-base@
-- library. This records all of the GHC flags used when building
-- @singletons-base@ so that when the test suite invokes GHC, it can find the
-- locally built version of @singletons-base@ and its dependencies.
module Main (main) where

import Data.List (isPrefixOf)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))


main :: IO ()
main = do
  -- The directory in which singletons-base and its test suite are located. This
  -- only works under the assumption that cabal will navigate to that directory
  -- before invoking the code generator. This is always the case when I've
  -- tested it, but if this assumption does not hold in general, we may need to
  -- revisit this assumption.
  singletonsBaseDir <- getCurrentDirectory
  args <- getArgs
  (tgt, rest) <-
    case args of
      (tgt:allFlags) -> pure (tgt, allFlags)
      [] -> fail "Expected at least one argument for code generator"
  ghcFlags <- takeGhcArgs rest
  -- Filter out GHC language extensions and warnings, as the singletons-base
  -- test suite wants to have finer-grained control over these.
  let ghcFlags' = filter (\flag -> not (isLangExtension flag || isWarning flag)) ghcFlags
  createDirectoryIfMissing True tgt
  writeFile (tgt </> generatedFileName <.> "hs") $ unlines
    [ "module " ++ generatedFileName ++ " where"
    , ""
    , "ghcFlags :: [String]"
    , "ghcFlags = " ++ show ghcFlags'
    , ""
    , "rootDir :: FilePath"
    , "rootDir = " ++ show singletonsBaseDir
    ]
  putStrLn generatedFileName

-- | @cabal@ code generators have a convention that GHC-specific arguments are
-- separated from the rest of the @cabal@-specific arguments using @--@.
-- Assuming this convention, this function looks up the GHC-specific arguments.
takeGhcArgs :: [String] -> IO [String]
takeGhcArgs ("--":xs) = pure xs
takeGhcArgs (_:xs)    = takeGhcArgs xs
takeGhcArgs []        = fail "Expected -- to separate arguments"

-- | Returns 'True' if a GHC command-line argument corresponds to a language
-- extension (e.g., @-XTypeFamilies@).
isLangExtension :: String -> Bool
isLangExtension = isPrefixOf "-X"

-- | Returns 'True' if a GHC command-line argument corresponds to a warning flag
-- (e.g., @-Wtabs@).
isWarning :: String -> Bool
isWarning flag = any (`isPrefixOf` flag) ["-W", "-fwarn", "-fno-warn"]

-- | The name of the generated file containing the GHC flags.
generatedFileName :: String
generatedFileName = "SingletonsBaseGHCFlags"
