{- Main.hs

(c) Richard Eisenberg 2012
eir@cis.upenn.edu

This file contains test cases for the singletons library. It should
compile with a warning that declarations containing badPlus are omitted.

-}

{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, UndecidableInstances,
             RankNTypes, TypeOperators, KindSignatures, FlexibleInstances,
             PolyKinds, DataKinds, FlexibleContexts, ConstraintKinds
 #-}

module Test.Main where

import Language.Haskell.TH hiding (Pred)

import Prelude hiding (print, Left, Right, (+), Maybe, Just, Nothing)
import Singletons.Lib hiding (SLeft, SRight, SJust, SNothing, sLeft, sRight, sJust, sNothing)
import Singletons.CustomStar

$(singletons [d| 
  data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat
      deriving (Eq, Show, Read)

  data NatBinTree where
    Leaf :: Nat -> NatBinTree
    Left :: Nat -> NatBinTree -> NatBinTree
    Right :: Nat -> NatBinTree -> NatBinTree
    Parent :: Nat -> NatBinTree -> NatBinTree -> NatBinTree
    deriving Eq

  data Foo where
    FLeaf :: Foo
    (:+:) :: Foo -> Foo -> Foo

  data Maybe a = Nothing | Just a deriving Eq

  data Box a = FBox a
  unBox :: Box a -> a
  unBox (FBox a) = a

  child :: Foo -> Foo
  child FLeaf = FLeaf
  child (a :+: _) = a

  pred :: Nat -> Nat
  pred Zero = Zero
  pred (Succ n) = n

  badPlus Zero m = m
  badPlus (Succ n) m = Succ (plus n m)

  plus :: Nat -> Nat -> Nat
  plus Zero m = m
  plus (Succ n) m = Succ (plus n m)

  (+) :: Nat -> Nat -> Nat
  Zero + m = m
  (Succ n) + m = Succ (n + m)

  mult :: Nat -> Nat -> Nat
  mult Zero m = Zero
  mult (Succ n) m = plus m (mult n m)
  
  treePlus :: Nat -> NatBinTree -> NatBinTree
  treePlus n (Leaf a) = Leaf (plus n a)
  treePlus n (Left a l) = Left (plus n a) (treePlus n l)
  treePlus n (Right a r) = Right (plus n a) (treePlus n r)
  treePlus n (Parent a l r) = Parent (plus n a) (treePlus n l) (treePlus n r)

  treeMap :: (Nat -> Nat) -> NatBinTree -> NatBinTree
  treeMap f (Leaf a) = Leaf (f a)
  treeMap f (Left a l) = Left (f a) (treeMap f l)
  treeMap f (Right a r) = Right (f a) (treeMap f r)
  treeMap f (Parent a l r) = Parent (f a) (treeMap f l) (treeMap f r)

  isJust :: Maybe a -> Bool
  isJust Nothing = False
  isJust (Just _) = True

  maybePlus :: Maybe Nat -> Maybe Nat
  maybePlus (Just n) = Just (plus (Succ Zero) n)
  maybePlus foo@Nothing = foo

  liftMaybe :: (a -> b) -> Maybe a -> Maybe b
  liftMaybe f (Just x) = Just (f x)
  liftMaybe _ Nothing = Nothing

  map :: (a -> b) -> [a] -> [b]
  map _ [] = []
  map f (h:t) = (f h) : (map f t)

  data Pair a b = Pair a b
  |])

$(singletons [d|
  pr = Pair (Succ Zero) (Leaf Zero)

  complex = Pair (Pair (Just Zero) Zero) False

  tuple = (False, Just Zero, True)

  aList = [Zero, Succ Zero, Succ (Succ Zero)]
 
  |])

$(promote [d|
  Pair sz lz = pr
  Pair (Pair jz zz) fls = complex
  (tf, tjz, tt) = tuple
  [_, lsz, (Succ blimy)] = aList

  infinite :: [Bool]
  infinite = (False : infinite)
  |])

$(singletons [d|
  -- used to test the "num args" feature of promoteDec
  returnFunc :: Nat -> Nat -> Nat
  returnFunc _ = Succ
  |])

data Vec :: * -> Nat -> * where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

$(singletonStar [''Nat, ''Int, ''String, ''Maybe, ''Vec])

instance Show (Sing (a :: Nat)) where
  show SZero = "SZero"
  show (SSucc n) = "(SSucc " ++ (show n) ++ ")"

instance Show (Sing (a :: NatBinTree)) where
  show (SLeaf a) = "(SLeaf " ++ (show a) ++ ")"
  show (SLeft a l) = "(SLeft " ++ (show a) ++ " " ++ (show l) ++ ")"
  show (SRight a r) = "(SRight " ++ (show a) ++ " " ++ (show r) ++ ")"
  show (SParent a l r) = "(SParent " ++ (show a) ++ " " ++ (show l) ++
                         " " ++ (show r) ++ ")"

one = SSucc SZero
two = SSucc one
three = SSucc two
four = SSucc three

tree = SParent one (SLeft two (SLeaf three)) (SLeaf four)
