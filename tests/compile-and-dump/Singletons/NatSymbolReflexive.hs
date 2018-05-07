module NatSymbolReflexive where

import Data.Singletons.Prelude
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits

test1 :: forall (a :: Nat). Sing a -> (a == a) :~: True
test1 _ = Refl

test2 :: forall (a :: Symbol). Sing a -> (a == a) :~: True
test2 _ = Refl
