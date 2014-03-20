{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes,
             TypeOperators, UndecidableInstances #-}
module ByHandDefunc where

import Data.Singletons.TH

--TyFun Nat (TyFun Nat (TyFun Nat Nat))

data Nat = Zero | Succ Nat deriving (Eq)

type instance (:==) Zero Zero = True
type instance (:==) Zero (Succ b) = False
type instance (:==) (Succ a) Zero = False
type instance (:==) (Succ a) (Succ b) = (:==) a b

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

type family Pred (a :: Nat) :: Nat
type instance Pred Zero = Zero
type instance Pred (Succ n) = n

data PredSym0 :: (TyFun Nat Nat) -> *
type instance Apply PredSym0 x = Pred x

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

type family Plus (m :: Nat) (n :: Nat) :: Nat
type instance Plus Zero n = n
type instance Plus (Succ n) m = Succ (Plus n m)

data PlusSym0 :: (TyFun Nat (TyFun Nat Nat -> *)) -> *
data PlusSym1 :: Nat -> (TyFun Nat Nat) -> *
type instance Apply PlusSym0 n     = PlusSym1 n
type instance Apply (PlusSym1 n) m = Plus n m

foo :: Nat -> Nat -> Nat -> Nat
foo Zero n l = plus n l
foo (Succ m) n l = Succ (foo m n l)

type family Foo (m :: Nat) (n :: Nat) (l :: Nat) :: Nat
type instance Foo Zero n l = Plus n l
type instance Foo (Succ m) n l = Succ (Foo m n l)

data FooSym0 :: TyFun Nat (TyFun Nat (TyFun Nat Nat -> *) -> *) -> *
data FooSym1 :: Nat -> (TyFun Nat (TyFun Nat Nat -> *)) -> *
data FooSym2 :: Nat -> Nat -> (TyFun Nat Nat) -> *
type instance Apply FooSym0 n       = FooSym1 n
type instance Apply (FooSym1 n) m   = FooSym2 n m
type instance Apply (FooSym2 n m) l = Foo n m l
