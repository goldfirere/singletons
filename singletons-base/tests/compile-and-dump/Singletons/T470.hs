module T470 where

import Data.Kind
import Data.Singletons.Base.TH

$(singletons [d|
  type T :: Type -> Type
  data T a where
    MkT1 :: a -> T a
    MkT2 :: !Void -> T a

  data S = MkS {-# UNPACK #-} !Bool
  |])

f :: T a -> a
f (MkT1 x) = x

type F :: T a -> a
type family F x where
  F ('MkT1 x) = x

sF :: forall a (x :: T a).
      Sing x -> Sing (F x)
sF (SMkT1 sx) = sx
