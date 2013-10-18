{- InsertionSortImp.hs

(c) Richard Eisenberg 2012
eir@cis.upenn.edu

This file contains an implementation of insertion sort over natural numbers,
along with a Haskell proof that the sort algorithm is correct. The code below
uses a combination of GADTs and class instances to record the progress and
result of the proof.

Ideally, the GADTs would be defined so that the constructors take no explicit
parameters --- the information would all be encoded in the constraints to the
constructors. However, due to the nature of the permutation relation, a class
instance definition corresponding to the constructor PermIns would require
existentially-quantified type variables (the l2 variable in the declaration of
PermIns). Type variables in an instance constraint but not mentioned in the
instance head are inherently ambiguous. The compiler would never be able to
infer the value of the variables. Thus, it is not possible to make a class
PermutationC analogous to PermutationProof in the way that AscendingC is
analogous to AscendingProof. (Note that it may be possible to fundamentally
rewrite the inductive definition of the permutation relation to avoid
existentially-quantified variables. We have not attempted that here.)

If there were a way to offer an explicit dictionary when satisfying a constraint,
this problem could be avoided, as the variable in question could be made
unambiguous.

-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, KindSignatures, DataKinds,
             MultiParamTypeClasses, GADTs, TypeFamilies, FlexibleInstances,
             UndecidableInstances, RankNTypes, ScopedTypeVariables,
             PolyKinds, FlexibleContexts, IncoherentInstances #-}

module Test.InsertionSortImp where

import Data.Singletons.TH
import Data.Singletons.Prelude

-- We use the Dict data type from Edward Kmett's constraints package to be
-- able to return dictionaries from functions
import Data.Constraint

-- Natural numbers, defined with singleton counterparts
$(singletons [d|
  data Nat = Zero | Succ Nat
  |])

-- convenience functions for testing purposes
toNat :: Int -> Nat
toNat 0         = Zero
toNat n | n > 0 = Succ (toNat (n - 1))
toNat _         = error "Converting negative to Nat"

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Succ n) = 1 + (fromNat n)

-- A less-than-or-equal relation among naturals
class (a :: Nat) :<=: (b :: Nat)
instance Zero :<=: a
instance (a :<=: b) => (Succ a) :<=: (Succ b)

-- A proof term asserting that a list of naturals is in ascending order
data AscendingProof :: [Nat] -> * where
  AscEmpty :: AscendingProof '[]
  AscOne :: AscendingProof '[n]
  AscCons :: (a :<=: b, AscendingC (b ': rest)) => AscendingProof (a ': b ': rest)

-- The class constraint (implicit parameter definition) corresponding to
-- AscendingProof
class AscendingC (lst :: [Nat]) where
  ascendingProof :: AscendingProof lst

-- The instances correspond to the constructors of AscendingProof
instance AscendingC '[] where
  ascendingProof = AscEmpty
instance AscendingC '[n] where
  ascendingProof = AscOne
instance (a :<=: b, AscendingC (b ': rest)) => AscendingC (a ': b ': rest) where
  ascendingProof = AscCons

-- A proof term asserting that l2 is the list produced when x is inserted
-- (anywhere) into list l1
data InsertionProof x l1 l2 where -- No kind signature possible due to bug #6049
  InsHere :: InsertionProof x l (x ': l)
  InsLater :: InsertionC x l1 l2 => InsertionProof x (y ': l1) (y ': l2)

-- The class constraint corresponding to InsertionProof
class InsertionC (x :: k) (l1 :: [k]) (l2 :: [k]) where
  insertionProof :: InsertionProof x l1 l2

instance InsertionC x l (x ': l) where
  insertionProof = InsHere
instance InsertionC x l1 l2 => InsertionC x (y ': l1) (y ': l2) where
  insertionProof = InsLater

-- A proof term asserting that l1 and l2 are permutations of each other
data PermutationProof l1 l2 where -- No kind signature due to bug #6049
  PermId :: PermutationProof l l
  PermIns :: InsertionC x l2 l2' => PermutationProof l1 l2 ->
               PermutationProof (x ': l1) l2'

-- Here is the definition of insertion sort about which we will be reasoning:
$(singletons [d|
  leq :: Nat -> Nat -> Bool
  leq Zero _ = True
  leq (Succ _) Zero = False
  leq (Succ a) (Succ b) = leq a b

  insert :: Nat -> [Nat] -> [Nat]
  insert n [] = [n]
  insert n (h:t) = if leq n h then (n:h:t) else h:(insert n t)

  insertionSort :: [Nat] -> [Nat]
  insertionSort [] = []
  insertionSort (h:t) = insert h (insertionSort t)
  |])

-- A lemma that states if sLeq a b is STrue, then (a :<=: b)
-- This is necessary to convert from the boolean definition of <= to the
-- corresponding constraint
sLeq_true__le :: (Leq a b ~ True) => SNat a -> SNat b -> Dict (a :<=: b)
sLeq_true__le a b = case (a, b) of
  (SZero, SZero) -> Dict
  (SZero, SSucc _) -> Dict
  -- (SSucc _, SZero) -> undefined <== IMPOSSIBLE
  (SSucc a', SSucc b') -> case sLeq_true__le a' b' of
    Dict -> Dict
  _ -> error "type checking failed"

-- A lemma that states if sLeq a b is SFalse, then (b :<=: a)
sLeq_false__nle :: (Leq a b ~ False) => SNat a -> SNat b -> Dict (b :<=: a)
sLeq_false__nle a b = case (a, b) of
  -- (SZero, SZero) -> undefined <== IMPOSSIBLE
  -- (SZero, SSucc _) -> undefined <== IMPOSSIBLE
  (SSucc _, SZero) -> Dict
  (SSucc a', SSucc b') -> case sLeq_false__nle a' b' of
    Dict -> Dict
  _ -> error "type checking failed"

-- A lemma that states that inserting into an ascending list produces an
-- ascending list
insert_ascending :: forall n lst.
  AscendingC lst => SNat n -> SList lst -> Dict (AscendingC (Insert n lst))
insert_ascending n lst =
  case ascendingProof :: AscendingProof lst of
    AscEmpty -> Dict -- If lst is empty, then we're done
    AscOne -> case lst of -- If lst has one element...
      -- SNil -> undefined <== IMPOSSIBLE
      SCons h _ -> case sLeq n h of -- then check if n is <= h
        STrue -> case sLeq_true__le n h of Dict -> Dict -- if so, we're done
        SFalse -> case sLeq_false__nle n h of Dict -> Dict -- if not, we're done
      _ -> error "type checking failed"
    AscCons -> case lst of -- Otherwise, if lst is more than one element...
      -- SNil -> undefined <== IMPOSSIBLE
      SCons h t -> case sLeq n h of -- then check if n is <= h
        STrue -> case sLeq_true__le n h of Dict -> Dict -- if so, we're done
        SFalse -> case sLeq_false__nle n h of -- if not, things are harder...
          Dict -> case t of -- destruct t: lst is (h : h2 : t2)
            -- SNil -> undefined <== IMPOSSIBLE
            SCons h2 _ -> case sLeq n h2 of -- is n <= h2?
              STrue -> -- if so, we're done
                case sLeq_true__le n h2 of Dict -> Dict
              SFalse -> -- otherwise, show that (Insert n t) is sorted
                case insert_ascending n t of Dict -> Dict -- and we're done
            _ -> error "type checking failed"
      _ -> error "type checking failed"

-- A lemma that states that inserting n into lst produces a new list with n
-- inserted into lst.
insert_insertion :: SNat n -> SList lst -> Dict (InsertionC n lst (Insert n lst))
insert_insertion n lst =
  case lst of
    SNil -> Dict -- if lst is empty, we're done
    SCons h t -> case sLeq n h of -- otherwise, is n <= h?
      STrue -> Dict -- if so, we're done
      SFalse -> case insert_insertion n t of Dict -> Dict -- otherwise, recur

-- A lemma that states that the result of an insertion sort is in ascending order
insertionSort_ascending :: SList lst -> Dict (AscendingC (InsertionSort lst))
insertionSort_ascending lst = case lst of
  SNil -> Dict -- if the list is empty, we're done

  -- otherwise, we recur to find that insertionSort on t produces an ascending list,
  -- and then we use the fact that inserting into an ascending list produces an
  -- ascending list
  SCons h t -> case insertionSort_ascending t of
    Dict -> case insert_ascending h (sInsertionSort t) of Dict -> Dict

-- A lemma that states that the result of an insertion sort is a permutation
-- of its input
insertionSort_permutes :: SList lst -> PermutationProof lst (InsertionSort lst)
insertionSort_permutes lst = case lst of
  SNil -> PermId -- if the list is empty, we're done

  -- otherwise, we wish to use PermIns. We must know that t is a permutation of
  -- the insertion sort of t and that inserting h into the insertion sort of t
  -- works correctly:
  SCons h t -> 
    case insert_insertion h (sInsertionSort t) of
      Dict -> PermIns (insertionSort_permutes t)

-- A theorem that states that the insertion sort of a list is both ascending
-- and a permutation of the original
insertionSort_correct :: SList lst -> (Dict (AscendingC (InsertionSort lst)),
                                       PermutationProof lst (InsertionSort lst))
insertionSort_correct lst = (insertionSort_ascending lst,
                             insertionSort_permutes lst)
