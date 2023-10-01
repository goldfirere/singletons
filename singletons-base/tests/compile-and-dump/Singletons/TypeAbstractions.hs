module TypeAbstractions where

import Data.Kind
import Data.Proxy
import Data.Proxy.Singletons
import Data.Singletons.Base.TH
import Data.Singletons.TH.Options
import Prelude.Singletons

$(withOptions defaultOptions{genSingKindInsts = False} $
  singletons [d|
  type D1 :: forall j k. j -> k -> Type
  data D1 @j @k (a :: j) (b :: k) = MkD1 (Proxy a) (Proxy b)

  type D2 :: forall j k. j -> k -> Type
  data D2 @x @y (a :: x) (b :: y) = MkD2 (Proxy a) (Proxy b)

  type D3 :: forall j. j -> forall k. k -> Type
  data D3 @j (a :: j) @k (b :: k) = MkD3 (Proxy a) (Proxy b)

  type D4 :: forall (a :: Type). Type
  data D4 @a = MkD4 a

  type C1 :: forall j k. j -> k -> Constraint
  class C1 @j @k (a :: j) (b :: k) where
    meth1 :: Proxy a -> Proxy b

  type C2 :: forall j k. j -> k -> Constraint
  class C2 @x @y (a :: x) (b :: y) where
    meth2 :: Proxy a -> Proxy b

  type C3 :: forall j. j -> forall k. k -> Constraint
  class C3 @j (a :: j) @k (b :: k) where
    meth3 :: Proxy a -> Proxy b

  type C4 :: forall (a :: Type). Constraint
  class C4 @a where
    meth4 :: a

  type TF :: forall j. j -> forall k. k -> Type
  type family TF @j (a :: j) @k (b :: k) where
    TF @j _ @k _ = (j, k)

  type TS :: forall j. j -> forall k. k -> Type
  type TS @j (a :: j) @k (b :: k) = (j, k)
  |])
