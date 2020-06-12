module Singletons.Star where

import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.Prelude.CustomStar
import Singletons.Nat
import Data.Kind (Type)

data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

$(singletonStar [''Nat, ''Int, ''String, ''Maybe, ''Vec])
