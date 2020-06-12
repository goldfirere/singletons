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

  infixr 5 `D1`, `MkD1`, `d1A`, `d1B`
  data D1 a b = MkD1 { d1A :: a, d1B :: b }
  |])

infix 5 `C2`
class C2 a b where
  infix 6 `m2`
  m2 :: a -> b -> Bool

infixl 5 `T2a`, `T2b`
type T2a a b = Either a b
type family T2b a b where
  T2b a b = Either a b

infixr 5 `D2`, `MkD2`, `d2A`, `d2B`
data D2 a b = MkD2 { d2A :: a, d2B :: b }

$(genSingletons [''C2, ''T2a, ''T2b, ''D2])
