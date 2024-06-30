module T601a where

import Data.Kind
import Data.Singletons.Base.TH
import Prelude.Singletons

$(promote [d|
  type MyApplicative :: (Type -> Type) -> Constraint
  class Functor f => MyApplicative f where
    ap :: f (a -> b) -> f a -> f b

    rightSparrow :: f a -> f b -> f b
    rightSparrow x y = ap (id <$ x) y
  |])
