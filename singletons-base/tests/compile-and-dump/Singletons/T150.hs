module T150 where

import Control.Monad.Trans.Class
import Data.Kind
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Singletons.Nat

-- singletons-th can generate SingKind instances for the following GADTs without
-- issues.
$(singletons
    [d| type Fin :: Nat -> Type
        data Fin n where
          FZ :: Fin (Succ n)
          FS :: Fin n -> Fin (Succ n)

        type Foo :: Type -> Type
        data Foo a where
          MkFoo1 :: Foo Bool
          MkFoo2 :: Foo Ordering

        type Vec :: Nat -> Type -> Type
        data Vec n a where
          VNil  :: Vec Zero a
          VCons :: a -> Vec n a -> Vec (Succ n) a

        headVec :: Vec (Succ n) a -> a
        headVec (VCons x _) = x

        tailVec :: Vec (Succ n) a -> Vec n a
        tailVec (VCons _ xs) = xs

        (!) :: Vec n a -> Fin n -> a
        VCons x _  ! FZ   = x
        VCons _ xs ! FS n = xs ! n
        VNil       ! n    = case n of {}

        mapVec :: (a -> b) -> Vec n a -> Vec n b
        mapVec _ VNil         = VNil
        mapVec f (VCons x xs) = VCons (f x) (mapVec f xs)

        type Equal :: Type -> Type -> Type
        data Equal a b where
          Reflexive :: Equal a a

        symmetry :: Equal a b -> Equal b a
        symmetry Reflexive = Reflexive

        transitivity :: Equal a b -> Equal b c -> Equal a c
        transitivity Reflexive Reflexive = Reflexive

        type HList :: [Type] -> Type
        data HList l where
          HNil  :: HList '[]
          HCons :: x -> HList xs -> HList (x:xs)

        type Prox :: k -> Type
        data Prox a where
          P :: forall k (a :: k). Prox a
    |])

-- The following GADTs are still too complicated for singletons-th to generate
-- SingKind instances for.
$(withOptions defaultOptions{genSingKindInsts = False} $
    singletons $ lift
    [d| type Obj :: Type
        data Obj where
          Obj :: a -> Obj

        type Prod :: (k -> Type) -> (k -> Type) -> k -> Type
        data Prod f g a = Prod (f a) (g a)

        type Summ :: (k -> Type) -> (k -> Type) -> k -> Type
        data Summ f g a = Inl (f a) | Inr (g a)

        type Comp :: (k -> Type) -> (j -> k) -> j -> Type
        newtype Comp f g a = Comp { getComp :: f (g a) }
    |])
