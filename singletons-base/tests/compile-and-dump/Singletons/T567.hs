module T567 where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Singletons.TH.Options
import Data.Singletons.Base.TH

$(withOptions defaultOptions{genSingKindInsts = False} $
  singletons [d|
  type D1 :: forall k. forall (a :: k) -> Proxy a -> Type
  data D1 x p = MkD1

  type D2 :: forall k. forall (a :: k) -> Proxy a -> Type
  data D2 (x :: j) p = MkD2

  type D3 :: forall k. forall (a :: k) -> Proxy a -> Type
  data D3 x (p :: Proxy x) = MkD3

  type D4 :: forall k. forall (a :: k) -> Proxy a -> Type
  data D4 (x :: j) (p :: Proxy x) = MkD4
  |])
