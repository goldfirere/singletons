module T271 where

import Data.Kind (Type)
import Data.Singletons.Base.TH

$(singletons
    [d| newtype Constant (a :: Type) (b :: Type) =
          Constant a deriving (Eq, Ord)

        data Identity :: Type -> Type where
          Identity :: a -> Identity a
          deriving (Eq, Ord)
      |])
