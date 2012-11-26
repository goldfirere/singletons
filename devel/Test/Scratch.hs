{-# LANGUAGE GADTs #-}

module Test.Scratch where

import Singletons.Lib

-- foo :: Sing a -> ()
foo a = case a of
  SNil -> ()
  SCons _ _ -> ()