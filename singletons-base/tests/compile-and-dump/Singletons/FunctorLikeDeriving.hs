{-# LANGUAGE DeriveTraversable #-}
-- Ensure that we can derive Functor, Foldable, and Traversable using only
-- an import of Data.Singletons.TH
module FunctorLikeDeriving where

import Data.Kind
import Data.Singletons.Base.TH

$(singletons [d|
  data T x a
    = MkT1 x a (Maybe a) (Maybe (Maybe a))
    | MkT2 (Maybe x)
    deriving (Functor, Foldable, Traversable)

  data Empty (a :: Type)
    deriving (Functor, Foldable, Traversable)
  |])
