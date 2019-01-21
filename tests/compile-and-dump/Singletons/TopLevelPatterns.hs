{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Singletons.TopLevelPatterns where

import Data.Singletons
import Data.Singletons.Prelude.List
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.TH hiding (SBool(..), TrueSym0, FalseSym0)

$(singletons [d|
  data Bool = False | True
  data Foo = Bar Bool Bool
 |])

$(singletons [d|
  otherwise :: Bool
  otherwise = True

  id :: a -> a
  id x = x

  not :: Bool -> Bool
  not True  = False
  not False = True

  false_ = False

  f,g :: Bool -> Bool
  [f,g] = [not, id]

  h,i :: Bool -> Bool
  (h,i) = (f, g)

  j,k :: Bool
  (Bar j k) = Bar True (h False)

  l,m :: Bool
  [l,m] = [not True, id False]
 |])
