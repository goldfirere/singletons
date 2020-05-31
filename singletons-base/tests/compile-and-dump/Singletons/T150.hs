module T150 where

import Control.Monad.Trans.Class
import Data.Kind
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Singletons.Nat

$(withOptions defaultOptions{genSingKindInsts = False} $
    singletons $ lift
    [d| data Fin :: Nat -> Type where
          FZ :: Fin (Succ n)
          FS :: Fin n -> Fin (Succ n)

        data Foo :: Type -> Type where
          MkFoo1 :: Foo Bool
          MkFoo2 :: Foo Ordering

        data Vec :: Nat -> Type -> Type where
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

        data Equal :: Type -> Type -> Type where
          Reflexive :: Equal a a

        symmetry :: Equal a b -> Equal b a
        symmetry Reflexive = Reflexive

        transitivity :: Equal a b -> Equal b c -> Equal a c
        transitivity Reflexive Reflexive = Reflexive

        data HList :: [Type] -> Type where
          HNil  :: HList '[]
          HCons :: x -> HList xs -> HList (x:xs)

        data Obj :: Type where
          Obj :: a -> Obj
    |])
