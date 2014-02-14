module Singletons.Star where

import Data.Singletons.CustomStar
import Data.Singletons.Prelude
import Data.Singletons.TH
import Singletons.Nat

data Vec :: * -> Nat -> * where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

$(singletonStar [''Nat, ''Int, ''String, ''Maybe, ''Vec])
