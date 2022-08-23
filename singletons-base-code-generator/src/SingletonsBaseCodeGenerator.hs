-- | TODO RGS: Docs
module Main (main) where

import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))

main :: IO ()
main = do
  args <- getArgs
  (tgt, rest) <-
    case args of
      (tgt:allFlags) -> pure (tgt, allFlags)
      [] -> fail "Expected at least one argument for code generator"
  ghcFlags <- takeGhcArgs rest
  let ghcFlags' = filter (\flag -> not (isLangExtension flag || isWarning flag)) ghcFlags
  createDirectoryIfMissing True tgt
  writeFile (tgt </> generatedFileName <.> "hs") $ unlines
    [ "module " ++ generatedFileName ++ " where"
    , ""
    , "ghcFlags :: [String]"
    , "ghcFlags = " ++ show ghcFlags'
    -- TODO RGS: Find parent directory of singletons-base.cabal
    ]
  putStrLn generatedFileName

-- | TODO RGS: Docs
takeGhcArgs :: [String] -> IO [String]
takeGhcArgs ("--":xs) = pure xs
takeGhcArgs (_:xs)    = takeGhcArgs xs
takeGhcArgs []        = fail "Expected -- to separate arguments"

-- | TODO RGS: Docs
isLangExtension :: String -> Bool
isLangExtension ('-':'X':_) = True
isLangExtension _           = False

-- | TODO RGS: Docs
isWarning :: String -> Bool
isWarning ('-':'W':_)                             = True
isWarning ('-':'f':'w':'a':'r':'n':_)             = True
isWarning ('-':'f':'n':'o':'-':'w':'a':'r':'n':_) = True
isWarning _                                       = False

generatedFileName :: String
generatedFileName = "SingletonsBaseGHCFlags"
