module T605 where

import Data.Kind
import Data.Singletons.Base.TH
import Prelude.Singletons

$(promoteOnly [d|
  type Traversable' :: (Type -> Type) -> Constraint
  class (Functor t, Foldable t) => Traversable' t where
    traverse' :: Applicative f => (a -> f b) -> t a -> t (f b)
  |])
