{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Singletons.Star where

import Data.Singletons.Prelude
import Data.Singletons.Decide
import Data.Singletons.CustomStar
import Singletons.Nat

#if __GLASGOW_HASKELL__ >= 711
import Data.Kind
#endif

data Vec :: * -> Nat -> * where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

$(singletonStar [''Nat, ''Int, ''String, ''Maybe, ''Vec])
