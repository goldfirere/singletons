module T412 where

import Data.Singletons.TH

$(singletons [d|
  infix 5 `C1`
  class C1 a b where
    infix 6 `m1`
    m1 :: a -> b -> Bool

  infixl 5 `T1a`, `T1b`
  type T1a a b = Either a b
  type family T1b a b where
    T1b a b = Either a b

  infixr 5 `D1`, `MkD1`
  data D1 a b = MkD1 a b
  |])

infix 5 `C2`
class C2 a b where
  infix 6 `m2`
  m2 :: a -> b -> Bool

infixl 5 `T2a`, `T2b`
type T2a a b = Either a b
type family T2b a b where
  T2b a b = Either a b

infixr 5 `D2`, `MkD2`
data D2 a b = MkD2 a b

$(genSingletons [''C2, ''T2a, ''T2b, ''D2])
