module T589 where

import Data.Kind
import Data.Proxy
import Data.Proxy.Singletons
import Data.Singletons.Base.TH
import Prelude.Singletons

$(singletons [d|
  type C1 :: Type -> Constraint
  class C1 b where
    m1 :: forall a. a -> b -> a

  instance C1 Ordering where
    m1 x _ = x

  type C2 :: k -> Constraint
  class C2 b where
    m2 :: a -> Proxy b

  instance C2 Ordering where
    m2 _ = Proxy
  |])

-- Test some type variable orderings
m1Ex :: Bool -> Ordering -> Bool
m1Ex = m1 @Ordering @Bool

type M1Ex :: Bool
type M1Ex = M1 @Ordering @Bool True EQ

type M1Ex0 :: Bool ~> Ordering ~> Bool
type M1Ex0 = M1Sym0 @Ordering @Bool

type M1Ex1 :: Bool -> Ordering ~> Bool
type M1Ex1 = M1Sym1 @Ordering @Bool

type M1Ex2 :: Bool
type M1Ex2 = M1Sym2 @Ordering @Bool True EQ

sM1Ex :: forall (x :: Bool) (y :: Ordering).
         Sing x -> Sing y -> Sing (M1 @Ordering @Bool x y)
sM1Ex = sM1 @Ordering @Bool

m2Ex :: Bool -> Proxy Ordering
m2Ex = m2 @Type @Ordering @Bool

type M2Ex :: Proxy Ordering
type M2Ex = M2 @Type @Ordering @Bool True

type M2Ex0 :: Bool ~> Proxy Ordering
type M2Ex0 = M2Sym0 @Type @Ordering @Bool

type M2Ex1 :: Proxy Ordering
type M2Ex1 = M2Sym1 @Type @Ordering @Bool True

sM2Ex :: forall (x :: Bool).
         Sing x -> Sing (M2 @Type @Ordering @Bool x)
sM2Ex = sM2 @Type @Ordering @Bool
