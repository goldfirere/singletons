-- uses the `turtle` package

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = sh $ do
  (ExitSuccess, top_dir) <- shellStrict "git rev-parse --show-toplevel" empty
  test_dir  <- ls (fromText (Text.strip top_dir) </> "tests" </> "compile-and-dump")
  True <- testdir test_dir
  test_file <- ls test_dir
  Right test_file' <- return (toText test_file)
  let pat = plus (notChar '.') <* (text ".actual")
  name_base <- select (match pat test_file')

  template_exists <- testfile (fromText $ name_base <> ".ghc86.template")
  when (not template_exists) $ do
    liftIO $ Text.putStrLn ("No template for " <> name_base)
    empty  -- aborts

  cp (fromText $ name_base <> ".actual") (fromText $ name_base <> ".ghc86.template")
  liftIO $ Text.putStrLn ("Accepted " <> name_base)
