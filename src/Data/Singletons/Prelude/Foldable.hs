{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Foldable
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the 'Foldable' type class.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Foldable (
  PFoldable(..), SFoldable(..),

  FoldrM, sFoldrM,
  FoldlM, sFoldlM,

  Traverse_, sTraverse_,
  For_, sFor_,
  SequenceA_, sSequenceA_,
  Asum, sAsum,

  MapM_, sMapM_,
  ForM_, sForM_,
  Sequence_, sSequence_,
  Msum, sMsum,

  Concat, sConcat,
  ConcatMap, sConcatMap,
  And, sAnd,
  Or, sOr,
  Any, sAny,
  All, sAll,
  MaximumBy, sMaximumBy,
  MinimumBy, sMinimumBy,

  NotElem, sNotElem,
  Find, sFind,

  -- * Defunctionalization symbols
  FoldSym0, FoldSym1,
  FoldMapSym0, FoldMapSym1, FoldMapSym2,
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  Foldr'Sym0, Foldr'Sym1, Foldr'Sym2, Foldr'Sym3,
  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  Foldl'Sym0, Foldl'Sym1, Foldl'Sym2, Foldl'Sym3,
  Foldr1Sym0, Foldr1Sym1, Foldr1Sym2,
  Foldl1Sym0, Foldl1Sym1, Foldl1Sym2,
  ToListSym0, ToListSym1,
  NullSym0, NullSym1,
  LengthSym0, LengthSym1,
  ElemSym0, ElemSym1, ElemSym2,
  MaximumSym0, MaximumSym1,
  MinimumSym0, MinimumSym1,
  SumSym0, SumSym1,
  ProductSym0, ProductSym1,

  FoldrMSym0, FoldrMSym1, FoldrMSym2, FoldrMSym3,
  FoldlMSym0, FoldlMSym1, FoldlMSym2, FoldlMSym3,

  Traverse_Sym0, Traverse_Sym1, Traverse_Sym2,
  For_Sym0, For_Sym1, For_Sym2,
  SequenceA_Sym0, SequenceA_Sym1,
  AsumSym0, AsumSym1,

  MapM_Sym0, MapM_Sym1, MapM_Sym2,
  ForM_Sym0, ForM_Sym1, ForM_Sym2,
  Sequence_Sym0, Sequence_Sym1,
  MsumSym0, MsumSym1,

  ConcatSym0, ConcatSym1,
  ConcatMapSym0, ConcatMapSym1, ConcatMapSym2,
  AndSym0, AndSym1,
  OrSym0, OrSym1,
  AnySym0, AnySym1, AnySym2,
  AllSym0, AllSym1, AllSym2,
  MaximumBySym0, MaximumBySym1, MaximumBySym2,
  MinimumBySym0, MinimumBySym1, MinimumBySym2,

  NotElemSym0, NotElemSym1, NotElemSym2,
  FindSym0, FindSym1, FindSym2
  ) where

import Control.Applicative
import Control.Monad
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid hiding (All(..), Any(..), Endo(..), Product(..), Sum(..))
import qualified Data.Monoid as Monoid (All(..), Any(..), Product(..), Sum(..))
import Data.Singletons.Internal
import Data.Singletons.Prelude.Base
  hiding (Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, sFoldr)
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Either
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
  hiding (Foldl, FoldlSym0(..), FoldlSym1(..), FoldlSym2(..), FoldlSym3, sFoldl)
import Data.Singletons.Prelude.List.Internal.Disambiguation
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Monoid
  hiding ( AllSym0,     AllSym1
         , AnySym0,     AnySym1
         , ProductSym0, ProductSym1
         , SumSym0,     SumSym1 )
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
  hiding ( Max, MaxSym0, MaxSym1, MaxSym2, sMax
         , Min, MinSym0, MinSym1, MinSym2, sMin )
import Data.Singletons.Prelude.Semigroup.Internal
  hiding ( AllSym0(..),     AllSym1,     SAll
         , AnySym0(..),     AnySym1,     SAny
         , FirstSym0,       FirstSym1,   SFirst
         , LastSym0,        LastSym1,    SLast
         , ProductSym0(..), ProductSym1, SProduct
         , SumSym0(..),     SumSym1,     SSum )
import Data.Singletons.Promote
import Data.Singletons.Single
import Data.Singletons.TypeLits.Internal

newtype Endo a = Endo (a ~> a)
data SEndo :: forall a. Endo a -> Type where
  SEndo :: Sing x -> SEndo ('Endo x)
type instance Sing = SEndo
data EndoSym0 :: forall a. (a ~> a) ~> Endo a
type instance Apply EndoSym0 x = 'Endo x

$(singletonsOnly [d|
  instance Semigroup (Endo a) where
          Endo x <> Endo y = Endo (x . y)

  instance Monoid (Endo a) where
          mempty = Endo id
  |])

newtype MaxInternal a = MaxInternal (Maybe a)
data SMaxInternal :: forall a. MaxInternal a -> Type where
  SMaxInternal :: Sing x -> SMaxInternal ('MaxInternal x)
type instance Sing = SMaxInternal
$(genDefunSymbols [''MaxInternal])

newtype MinInternal a = MinInternal (Maybe a)
data SMinInternal :: forall a. MinInternal a -> Type where
  SMinInternal :: Sing x -> SMinInternal ('MinInternal x)
type instance Sing = SMinInternal
$(genDefunSymbols [''MinInternal])

$(singletonsOnly [d|
  instance Ord a => Semigroup (MaxInternal a) where
      m <> MaxInternal Nothing = m
      MaxInternal Nothing <> n = n
      (MaxInternal m@(Just x)) <> (MaxInternal n@(Just y))
        = if x >= y then MaxInternal m else MaxInternal n

  instance Ord a => Monoid (MaxInternal a) where
      mempty = MaxInternal Nothing

  instance Ord a => Semigroup (MinInternal a) where
      m <> MinInternal Nothing = m
      MinInternal Nothing <> n = n
      (MinInternal m@(Just x)) <> (MinInternal n@(Just y))
        = if x <= y then MinInternal m else MinInternal n

  instance Ord a => Monoid (MinInternal a) where
      mempty = MinInternal Nothing
  |])

$(singletonsOnly [d|
  -- -| Data structures that can be folded.
  --
  -- For example, given a data type
  --
  -- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  --
  -- a suitable instance would be
  --
  -- > instance Foldable Tree where
  -- >    foldMap f Empty = mempty
  -- >    foldMap f (Leaf x) = f x
  -- >    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
  --
  -- This is suitable even for abstract types, as the monoid is assumed
  -- to satisfy the monoid laws.  Alternatively, one could define @foldr@:
  --
  -- > instance Foldable Tree where
  -- >    foldr f z Empty = z
  -- >    foldr f z (Leaf x) = f x z
  -- >    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
  --
  -- @Foldable@ instances are expected to satisfy the following laws:
  --
  -- > foldr f z t = appEndo (foldMap (Endo . f) t ) z
  --
  -- > foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
  --
  -- > fold = foldMap id
  --
  -- > length = getSum . foldMap (Sum . const  1)
  --
  -- @sum@, @product@, @maximum@, and @minimum@ should all be essentially
  -- equivalent to @foldMap@ forms, such as
  --
  -- > sum = getSum . foldMap Sum
  --
  -- but may be less defined.
  --
  -- If the type is also a 'Functor' instance, it should satisfy
  --
  -- > foldMap f = fold . fmap f
  --
  -- which implies that
  --
  -- > foldMap f . fmap g = foldMap (f . g)

  class Foldable (t :: Type -> Type) where
      -- {-# MINIMAL foldMap | foldr #-}

      -- -| Combine the elements of a structure using a monoid.
      fold :: Monoid m => t m -> m
      fold = foldMap id

      -- -| Map each element of the structure to a monoid,
      -- and combine the results.
      foldMap :: Monoid m => (a -> m) -> t a -> m
      foldMap f = foldr (mappend . f) mempty

      -- -| Right-associative fold of a structure.
      --
      -- In the case of lists, 'foldr', when applied to a binary operator, a
      -- starting value (typically the right-identity of the operator), and a
      -- list, reduces the list using the binary operator, from right to left:
      --
      -- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
      --
      -- Note that, since the head of the resulting expression is produced by
      -- an application of the operator to the first element of the list,
      -- 'foldr' can produce a terminating expression from an infinite list.
      --
      -- For a general 'Foldable' structure this should be semantically identical
      -- to,
      --
      -- @foldr f z = 'List.foldr' f z . 'toList'@
      --
      foldr :: (a -> b -> b) -> b -> t a -> b
      foldr f z t = case foldMap (Endo . f) t of
                      Endo g -> g z

      -- -| Right-associative fold of a structure, but with strict application of
      -- the operator.
      --
      foldr' :: (a -> b -> b) -> b -> t a -> b
      foldr' f z0 xs = foldl f' id xs z0
        where f' k x z = k $! f x z

      -- -| Left-associative fold of a structure.
      --
      -- In the case of lists, 'foldl', when applied to a binary
      -- operator, a starting value (typically the left-identity of the operator),
      -- and a list, reduces the list using the binary operator, from left to
      -- right:
      --
      -- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
      --
      -- Note that to produce the outermost application of the operator the
      -- entire input list must be traversed. This means that 'foldl'' will
      -- diverge if given an infinite list.
      --
      -- Also note that if you want an efficient left-fold, you probably want to
      -- use 'foldl'' instead of 'foldl'. The reason for this is that latter does
      -- not force the "inner" results (e.g. @z `f` x1@ in the above example)
      -- before applying them to the operator (e.g. to @(`f` x2)@). This results
      -- in a thunk chain @O(n)@ elements long, which then must be evaluated from
      -- the outside-in.
      --
      -- For a general 'Foldable' structure this should be semantically identical
      -- to,
      --
      -- @foldl f z = 'List.foldl' f z . 'toList'@
      --
      foldl :: (b -> a -> b) -> b -> t a -> b
      foldl f z t = case foldMap (Dual . Endo . flip f) t of
                      Dual (Endo g) -> g z
      -- There's no point mucking around with coercions here,
      -- because flip forces us to build a new function anyway.

      -- -| Left-associative fold of a structure but with strict application of
      -- the operator.
      --
      -- This ensures that each step of the fold is forced to weak head normal
      -- form before being applied, avoiding the collection of thunks that would
      -- otherwise occur. This is often what you want to strictly reduce a finite
      -- list to a single, monolithic result (e.g. 'length').
      --
      -- For a general 'Foldable' structure this should be semantically identical
      -- to,
      --
      -- @foldl f z = 'List.foldl'' f z . 'toList'@
      --
      foldl' :: (b -> a -> b) -> b -> t a -> b
      foldl' f z0 xs = foldr f' id xs z0
        where f' x k z = k $! f z x

      -- -| A variant of 'foldr' that has no base case,
      -- and thus may only be applied to non-empty structures.
      --
      -- @'foldr1' f = 'List.foldr1' f . 'toList'@
      foldr1 :: (a -> a -> a) -> t a -> a
      foldr1 f xs = fromMaybe (errorWithoutStackTrace "foldr1: empty structure")
                      (foldr mf Nothing xs)
        where
          mf x m = Just (case m of
                           Nothing -> x
                           Just y  -> f x y)

      -- -| A variant of 'foldl' that has no base case,
      -- and thus may only be applied to non-empty structures.
      --
      -- @'foldl1' f = 'List.foldl1' f . 'toList'@
      foldl1 :: (a -> a -> a) -> t a -> a
      foldl1 f xs = fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
                      (foldl mf Nothing xs)
        where
          mf m y = Just (case m of
                           Nothing -> y
                           Just x  -> f x y)

      -- -| List of elements of a structure, from left to right.
      toList :: t a -> [a]
      toList = foldr (:) []

      -- -| Test whether the structure is empty. The default implementation is
      -- optimized for structures that are similar to cons-lists, because there
      -- is no general way to do better.
      null :: t a -> Bool
      null = foldr (\_ _ -> False) True

      -- -| Returns the size/length of a finite structure as an 'Int'.  The
      -- default implementation is optimized for structures that are similar to
      -- cons-lists, because there is no general way to do better.
      length :: t a -> Nat
      length = foldl' (\c _ -> c+1) 0

      -- -| Does the element occur in the structure?
      elem :: Eq a => a -> t a -> Bool
      elem = any . (==)

      -- -| The largest element of a non-empty structure.
      maximum :: forall a . Ord a => t a -> a
      maximum x =
        case foldMap (MaxInternal . Just) x of
          MaxInternal y -> fromMaybe (errorWithoutStackTrace "maximum: empty structure") y

      -- -| The least element of a non-empty structure.
      minimum :: forall a . Ord a => t a -> a
      minimum x =
        case foldMap (MinInternal . Just) x of
          MinInternal y -> fromMaybe (errorWithoutStackTrace "minimum: empty structure") y

      -- -| The 'sum' function computes the sum of the numbers of a structure.
      sum :: Num a => t a -> a
      sum x = case foldMap sum_ x of
                Monoid.Sum y -> y

      -- -| The 'product' function computes the product of the numbers of a
      -- structure.
      product :: Num a => t a -> a
      product x = case foldMap product_ x of
                    Monoid.Product y -> y

  -- instances for Prelude types

  instance Foldable Maybe where
      foldMap = maybe_ mempty

      foldr _ z Nothing = z
      foldr f z (Just x) = f x z

      foldl _ z Nothing = z
      foldl f z (Just x) = f z x

  instance Foldable [] where
      elem    = listelem
      foldl   = listfoldl
      foldl'  = listfoldl'
      foldl1  = listfoldl1
      foldr   = listfoldr
      foldr1  = listfoldr1
      length  = listlength
      maximum = listmaximum
      minimum = listminimum
      null    = listnull
      product = listproduct
      sum     = listsum
      toList  = id

  instance Foldable NonEmpty where
    foldr f z (a :| as) = f a (listfoldr f z as)
    foldl f z (a :| as) = listfoldl f (f z a) as
    foldl1 f (a :| as) = listfoldl f a as

    -- GHC isn't clever enough to transform the default definition
    -- into anything like this, so we'd end up shuffling a bunch of
    -- Maybes around.
    foldr1 f (p :| ps) = foldr go id ps p
      where
        go x r prev = f prev (r x)

    -- We used to say
    --
    --   length (_ :| as) = 1 + length as
    --
    -- but the default definition is better, counting from 1.
    --
    -- The default definition also works great for null and foldl'.
    -- As usual for cons lists, foldr' is basically hopeless.

    foldMap f (a :| as) = f a `mappend` foldMap f as
    fold (m :| ms) = m `mappend` fold ms
    toList (a :| as) = a : as

  instance Foldable (Either a) where
      foldMap _ (Left _) = mempty
      foldMap f (Right y) = f y

      foldr _ z (Left _) = z
      foldr f z (Right y) = f y z

      length (Left _)  = 0
      length (Right _) = 1

      null             = isLeft

  instance Foldable Dual where
      foldMap f (Dual x)  = f x

      elem x (Dual y)     = x == y
      foldl f z (Dual x)  = f z x
      foldl' f z (Dual x) = f z x
      foldl1 _ (Dual x)   = x
      foldr f z (Dual x)  = f x z
      foldr'              = foldr
      foldr1 _ (Dual x)   = x
      length _            = 1
      maximum (Dual x)    = x
      minimum (Dual x)    = x
      null _              = False
      product (Dual x)    = x
      sum (Dual x)        = x
      toList (Dual x)     = [x]

  instance Foldable Monoid.Sum where
      foldMap f (Monoid.Sum x)  = f x

      elem x (Monoid.Sum y)     = x == y
      foldl f z (Monoid.Sum x)  = f z x
      foldl' f z (Monoid.Sum x) = f z x
      foldl1 _ (Monoid.Sum x)   = x
      foldr f z (Monoid.Sum x)  = f x z
      foldr'                    = foldr
      foldr1 _ (Monoid.Sum x)   = x
      length _                  = 1
      maximum (Monoid.Sum x)    = x
      minimum (Monoid.Sum x)    = x
      null _                    = False
      product (Monoid.Sum x)    = x
      sum (Monoid.Sum x)        = x
      toList (Monoid.Sum x)     = [x]

  instance Foldable Monoid.Product where
      foldMap f (Monoid.Product x)  = f x

      elem x (Monoid.Product y)     = x == y
      foldl f z (Monoid.Product x)  = f z x
      foldl' f z (Monoid.Product x) = f z x
      foldl1 _ (Monoid.Product x)   = x
      foldr f z (Monoid.Product x)  = f x z
      foldr'              = foldr
      foldr1 _ (Monoid.Product x)   = x
      length _            = 1
      maximum (Monoid.Product x)    = x
      minimum (Monoid.Product x)    = x
      null _              = False
      product (Monoid.Product x)    = x
      sum (Monoid.Product x)        = x
      toList (Monoid.Product x)     = [x]

  -- -| Monadic fold over the elements of a structure,
  -- associating to the right, i.e. from right to left.
  foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
  foldrM f z0 xs = foldl f' return xs z0
    where f' k x z = f x z >>= k

  -- -| Monadic fold over the elements of a structure,
  -- associating to the left, i.e. from left to right.
  foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
  foldlM f z0 xs = foldr f' return xs z0
    where f' x k z = f z x >>= k

  -- -| Map each element of a structure to an action, evaluate these
  -- actions from left to right, and ignore the results. For a version
  -- that doesn't ignore the results see 'Data.Traversable.traverse'.
  traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
  traverse_ f = foldr ((*>) . f) (pure ())

  -- -| 'for_' is 'traverse_' with its arguments flipped. For a version
  -- that doesn't ignore the results see 'Data.Traversable.for'.
  --
  -- >>> for_ [1..4] print
  -- 1
  -- 2
  -- 3
  -- 4
  for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
  for_ = flip traverse_

  -- -| Map each element of a structure to a monadic action, evaluate
  -- these actions from left to right, and ignore the results. For a
  -- version that doesn't ignore the results see
  -- 'Data.Traversable.mapM'.
  --
  -- As of base 4.8.0.0, 'mapM_' is just 'traverse_', specialized to
  -- 'Monad'.
  mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
  mapM_ f= foldr ((>>) . f) (return ())

  -- -| 'forM_' is 'mapM_' with its arguments flipped. For a version that
  -- doesn't ignore the results see 'Data.Traversable.forM'.
  --
  -- As of base 4.8.0.0, 'forM_' is just 'for_', specialized to 'Monad'.
  forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
  forM_ = flip mapM_

  -- -| Evaluate each action in the structure from left to right, and
  -- ignore the results. For a version that doesn't ignore the results
  -- see 'Data.Traversable.sequenceA'.
  sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
  sequenceA_ = foldr (*>) (pure ())

  -- -| Evaluate each monadic action in the structure from left to right,
  -- and ignore the results. For a version that doesn't ignore the
  -- results see 'Data.Traversable.sequence'.
  --
  -- As of base 4.8.0.0, 'sequence_' is just 'sequenceA_', specialized
  -- to 'Monad'.
  sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
  sequence_ = foldr (>>) (return ())

  -- -| The sum of a collection of actions, generalizing 'concat'.
  --
  -- asum [Just "Hello", Nothing, Just "World"]
  -- Just "Hello"
  asum :: (Foldable t, Alternative f) => t (f a) -> f a
  asum = foldr (<|>) empty

  -- -| The sum of a collection of actions, generalizing 'concat'.
  -- As of base 4.8.0.0, 'msum' is just 'asum', specialized to 'MonadPlus'.
  msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
  msum = asum

  -- -| The concatenation of all the elements of a container of lists.
  concat :: Foldable t => t [a] -> [a]
  concat xs = foldr (\x y -> foldr (:) y x) [] xs

  -- -| Map a function over all the elements of a container and concatenate
  -- the resulting lists.
  concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
  concatMap f xs = foldr (\x b -> foldr (:) b (f x)) [] xs

  -- These use foldr rather than foldMap to avoid repeated concatenation.

  -- -| 'and' returns the conjunction of a container of Bools.  For the
  -- result to be 'True', the container must be finite; 'False', however,
  -- results from a 'False' value finitely far from the left end.
  and :: Foldable t => t Bool -> Bool
  and x = case foldMap all_ x of
            Monoid.All y -> y

  -- -| 'or' returns the disjunction of a container of Bools.  For the
  -- result to be 'False', the container must be finite; 'True', however,
  -- results from a 'True' value finitely far from the left end.
  or :: Foldable t => t Bool -> Bool
  or x = case foldMap any_ x of
           Monoid.Any y -> y

  -- -| Determines whether any element of the structure satisfies the predicate.
  any :: Foldable t => (a -> Bool) -> t a -> Bool
  any p x = case foldMap (any_ . p) x of
              Monoid.Any y -> y

  -- -| Determines whether all elements of the structure satisfy the predicate.
  all :: Foldable t => (a -> Bool) -> t a -> Bool
  all p x = case foldMap (all_ . p) x of
              Monoid.All y -> y

  -- -| The largest element of a non-empty structure with respect to the
  -- given comparison function.

  -- See Note [maximumBy/minimumBy space usage]
  maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
  maximumBy cmp = foldl1 max'
    where max' x y = case cmp x y of
                          GT -> x
                          LT -> y
                          EQ -> y

  -- -| The least element of a non-empty structure with respect to the
  -- given comparison function.

  -- See Note [maximumBy/minimumBy space usage]
  minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
  minimumBy cmp = foldl1 min'
    where min' x y = case cmp x y of
                          GT -> y
                          LT -> x
                          EQ -> x

  -- -| 'notElem' is the negation of 'elem'.
  notElem :: (Foldable t, Eq a) => a -> t a -> Bool
  notElem x = not . elem x

  -- -| The 'find' function takes a predicate and a structure and returns
  -- the leftmost element of the structure matching the predicate, or
  -- 'Nothing' if there is no such element.
  find :: Foldable t => (a -> Bool) -> t a -> Maybe a
  find p y = case foldMap (\ x -> First (if p x then Just x else Nothing)) y of
               First z -> z
  |])

$(singletonsOnly [d|
  -- instances for Prelude types (part 2)

  deriving instance Foldable ((,) a)
  deriving instance Foldable First
  deriving instance Foldable Last
  |])
