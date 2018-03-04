{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeInType, TypeOperators,
             TypeFamilies, GADTs, UndecidableInstances, InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.List.NonEmpty
-- Copyright   :  (C) 2016 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for 'NonEmpty',
-- including a singletons version of all the definitions in @Data.List.NonEmpty@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.List.NonEmpty@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.List.NonEmpty (
  -- * The 'NonEmpty' singleton

  Sing((:%|)),

  -- | Though Haddock doesn't show it, the 'Sing' instance above declares
  -- constructor
  --
  -- > (:%|) :: Sing h -> Sing t -> Sing (h :| t)

  SNonEmpty,
  -- | 'SNonEmpty' is a kind-restricted synonym for 'Sing':
  -- @type SNonEmpty (a :: NonEmpty) = Sing a@

  -- * Non-empty stream transformations
  Map, sMap,
  Intersperse, sIntersperse,
  Scanl, sScanl,
  Scanr, sScanr,
  Scanl1, sScanl1,
  Scanr1, sScanr1,
  Transpose, sTranspose,
  SortBy, sSortBy,
  SortWith, sSortWith,
  Length, sLength,
  Head, sHead,
  Tail, sTail,
  Last, sLast,
  Init, sInit,
  type (<|), (%<|),
  Cons, sCons,
  Uncons, sUncons,
  Unfoldr, sUnfoldr,
  Sort, sSort,
  Reverse, sReverse,
  Inits, sInits,
  Tails, sTails,
  Unfold, sUnfold,
  Insert, sInsert,
  Take, sTake,
  Drop, sDrop,
  SplitAt, sSplitAt,
  TakeWhile, sTakeWhile,
  DropWhile, sDropWhile,
  Span, sSpan,
  Break, sBreak,
  Filter, sFilter,
  Partition, sPartition,
  Group, sGroup,
  GroupBy, sGroupBy,
  GroupWith, sGroupWith,
  GroupAllWith, sGroupAllWith,
  Group1, sGroup1,
  GroupBy1, sGroupBy1,
  GroupWith1, sGroupWith1,
  GroupAllWith1, sGroupAllWith1,
  IsPrefixOf, sIsPrefixOf,
  Nub, sNub,
  NubBy, sNubBy,
  type (!!), (%!!),
  Zip, sZip,
  ZipWith, sZipWith,
  Unzip, sUnzip,
  FromList, sFromList,
  ToList, sToList,
  NonEmpty_, sNonEmpty_,
  Xor, sXor,

  -- * Defunctionalization symbols
  (:|@#@$), (:|@#@$$), (:|@#@$$$),
  MapSym0, MapSym1, MapSym2,
  IntersperseSym0, IntersperseSym1, IntersperseSym2,
  ScanlSym0, ScanlSym1, ScanlSym2, ScanlSym3,
  ScanrSym0, ScanrSym1, ScanrSym2, ScanrSym3,
  Scanl1Sym0, Scanl1Sym1, Scanl1Sym2,
  Scanr1Sym0, Scanr1Sym1, Scanr1Sym2,
  TransposeSym0, TransposeSym1,
  SortBySym0, SortBySym1, SortBySym2,
  SortWithSym0, SortWithSym1, SortWithSym2,
  LengthSym0, LengthSym1,
  HeadSym0, HeadSym1,
  TailSym0, TailSym1,
  LastSym0, LastSym1,
  InitSym0, InitSym1,
  type (<|@#@$), type (<|@#@$$), type (<|@#@$$$),
  ConsSym0, ConsSym1, ConsSym2,
  UnconsSym0, UnconsSym1,
  UnfoldrSym0, UnfoldrSym1, UnfoldrSym2,
  SortSym0, SortSym1,
  ReverseSym0, ReverseSym1,
  InitsSym0, InitsSym1,
  TailsSym0, TailsSym1,
  UnfoldSym0, UnfoldSym1,
  InsertSym0, InsertSym1, InsertSym2,
  TakeSym0, TakeSym1, TakeSym2,
  DropSym0, DropSym1, DropSym2,
  SplitAtSym0, SplitAtSym1, SplitAtSym2,
  TakeWhileSym0, TakeWhileSym1, TakeWhileSym2,
  DropWhileSym0, DropWhileSym1, DropWhileSym2,
  SpanSym0, SpanSym1, SpanSym2,
  BreakSym0, BreakSym1, BreakSym2,
  FilterSym0, FilterSym1, FilterSym2,
  PartitionSym0, PartitionSym1, PartitionSym2,
  GroupSym0, GroupSym1,
  GroupBySym0, GroupBySym1, GroupBySym2,
  GroupWithSym0, GroupWithSym1, GroupWithSym2,
  GroupAllWithSym0, GroupAllWithSym1, GroupAllWithSym2,
  Group1Sym0, Group1Sym1,
  GroupBy1Sym0, GroupBy1Sym1, GroupBy1Sym2,
  GroupWith1Sym0, GroupWith1Sym1, GroupWith1Sym2,
  GroupAllWith1Sym0, GroupAllWith1Sym1, GroupAllWith1Sym2,
  IsPrefixOfSym0, IsPrefixOfSym1, IsPrefixOfSym2,
  NubSym0, NubSym1,
  NubBySym0, NubBySym1, NubBySym2,
  type (!!@#@$), type (!!@#@$$), type (!!@#@$$$),
  ZipSym0, ZipSym1, ZipSym2,
  ZipWithSym0, ZipWithSym1, ZipWithSym2, ZipWithSym3,
  UnzipSym0, UnzipSym1,
  FromListSym0, FromListSym1,
  ToListSym0, ToListSym1,
  NonEmpty_Sym0, NonEmpty_Sym1,
  XorSym0, XorSym1
  ) where

import Control.Monad.Zip
import Data.List.NonEmpty
import Data.Singletons.Prelude.List.NonEmpty.Internal
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Base hiding ( MapSym0, MapSym1, MapSym2, Map, sMap )
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Monad.Zip
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Function
import Data.Function
import Data.Ord
import Data.Singletons.TypeLits
import Data.Singletons.Single

$(singletonsOnly [d|
  {-
  -- | @since 4.9.0.0
  instance Exts.IsList (NonEmpty a) where
    type Item (NonEmpty a) = a
    fromList               = fromList
    toList                 = toList

  -- | @since 4.9.0.0
  instance MonadFix NonEmpty where
    mfix f = case fix (f . head) of
               ~(x :| _) -> x :| mfix (tail . f)
  -}

  instance MonadZip NonEmpty where
    mzip     = zip
    mzipWith = zipWith
    munzip   = unzip

  -- needed to implement other functions
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| listmap f xs

  -- -| Number of elements in 'NonEmpty' list.
  length :: NonEmpty a -> Nat
  length (_ :| xs) = 1 + listlength xs

  -- -| Compute n-ary logic exclusive OR operation on 'NonEmpty' list.
  xor :: NonEmpty Bool -> Bool
  xor (x :| xs)   = foldr xor' x xs
    where xor' True y  = not y
          xor' False y = y

  -- -| 'unfold' produces a new stream by repeatedly applying the unfolding
  -- function to the seed value to produce an element of type @b@ and a new
  -- seed value.  When the unfolding function returns 'Nothing' instead of
  -- a new seed value, the stream ends.
  unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
  unfold f a = case f a of
    (b, Nothing) -> b :| []
    (b, Just c)  -> b <| unfold f c

  -- -| 'nonEmpty' efficiently turns a normal list into a 'NonEmpty' stream,
  -- producing 'Nothing' if the input is empty.
  nonEmpty_ :: [a] -> Maybe (NonEmpty a)
  nonEmpty_ []     = Nothing
  nonEmpty_ (a:as) = Just (a :| as)

  -- -| 'uncons' produces the first element of the stream, and a stream of the
  -- remaining elements, if any.
  uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
  uncons (a :| as) = (a, nonEmpty_ as)

  -- -| The 'unfoldr' function is analogous to "Data.List"'s
  -- 'Data.List.unfoldr' operation.
  unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
  unfoldr f a = case f a of
    (b, mc) -> b :| maybe_ [] go mc
   where
      go c = case f c of
        (d, me) -> d : maybe_ [] go me

  {-
  -- | @since 4.9.0.0
  instance Functor NonEmpty where
    fmap f ~(a :| as) = f a :| fmap f as
    b <$ ~(_ :| as)   = b   :| (b <$ as)

  -- | @since 4.9.0.0
  instance Applicative NonEmpty where
    pure a = a :| []
    (<*>) = ap

  -- | @since 4.9.0.0
  instance Monad NonEmpty where
    ~(a :| as) >>= f = b :| (bs ++ bs')
      where b :| bs = f a
            bs' = as >>= toList . f

  -- | @since 4.9.0.0
  instance Traversable NonEmpty where
    traverse f ~(a :| as) = (:|) <$> f a <*> traverse f as

  -- | @since 4.9.0.0
  instance Foldable NonEmpty where
    foldr f z ~(a :| as) = f a (foldr f z as)
    foldl f z ~(a :| as) = foldl f (f z a) as
    foldl1 f ~(a :| as) = foldl f a as
    foldMap f ~(a :| as) = f a `mappend` foldMap f as
    fold ~(m :| ms) = m `mappend` fold ms
  -}

  -- -| Extract the first element of the stream.
  head :: NonEmpty a -> a
  head (a :| _) = a

  -- -| Extract the possibly-empty tail of the stream.
  tail :: NonEmpty a -> [a]
  tail (_ :| as) = as

  -- -| Extract the last element of the stream.
  last :: NonEmpty a -> a
  last (a :| as) = listlast (a : as)

  -- -| Extract everything except the last element of the stream.
  init :: NonEmpty a -> [a]
  init (a :| as) = listinit (a : as)

  -- -| Prepend an element to the stream.
  (<|) :: a -> NonEmpty a -> NonEmpty a
  a <| (b :| bs) = a :| b : bs

  -- -| Synonym for '<|'.
  cons :: a -> NonEmpty a -> NonEmpty a
  cons = (<|)

  -- -| Sort a stream.
  sort :: Ord a => NonEmpty a -> NonEmpty a
  sort = lift listsort

  -- -| Converts a normal list to a 'NonEmpty' stream.
  --
  -- Raises an error if given an empty list.
  fromList :: [a] -> NonEmpty a
  fromList (a:as) = a :| as
  fromList [] = error "NonEmpty.fromList: empty list"

  -- -| Convert a stream to a normal list efficiently.
  toList :: NonEmpty a -> [a]
  toList (a :| as) = a : as

  -- -| Lift list operations to work on a 'NonEmpty' stream.
  --
  -- /Beware/: If the provided function returns an empty list,
  -- this will raise an error.
  lift :: ([a] -> [b]) -> NonEmpty a -> NonEmpty b
  lift f = fromList . f . toList

  -- -| Map a function over a 'NonEmpty' stream.
  map :: (a -> b) -> NonEmpty a -> NonEmpty b
  map f (a :| as) = f a :| listmap f as

  -- -| The 'inits' function takes a stream @xs@ and returns all the
  -- finite prefixes of @xs@.
  inits :: [a] -> NonEmpty [a]
  inits = fromList . listinits

  -- -| The 'tails' function takes a stream @xs@ and returns all the
  -- suffixes of @xs@.
  tails   :: [a] -> NonEmpty [a]
  tails = fromList . listtails

  -- -| @'insert' x xs@ inserts @x@ into the last position in @xs@ where it
  -- is still less than or equal to the next element. In particular, if the
  -- list is sorted beforehand, the result will also be sorted.
  insert  :: Ord a => a -> [a] -> NonEmpty a
  insert a = fromList . listinsert a

  {-
  -- | @'some1' x@ sequences @x@ one or more times.
  some1 :: Alternative f => f a -> f (NonEmpty a)
  some1 x = (:|) <$> x <*> many x
  -}

  -- -| 'scanl' is similar to 'foldl', but returns a stream of successive
  -- reduced values from the left:
  --
  -- > scanl f z [x1, x2, ...] == z :| [z `f` x1, (z `f` x1) `f` x2, ...]
  --
  -- Note that
  --
  -- > last (scanl f z xs) == foldl f z xs.
  scanl   :: (b -> a -> b) -> b -> [a] -> NonEmpty b
  scanl f z = fromList . listscanl f z

  -- -| 'scanr' is the right-to-left dual of 'scanl'.
  -- Note that
  --
  -- > head (scanr f z xs) == foldr f z xs.
  scanr   :: (a -> b -> b) -> b -> [a] -> NonEmpty b
  scanr f z = fromList . listscanr f z

  -- -| 'scanl1' is a variant of 'scanl' that has no starting value argument:
  --
  -- > scanl1 f [x1, x2, ...] == x1 :| [x1 `f` x2, x1 `f` (x2 `f` x3), ...]
  scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
  scanl1 f (a :| as) = fromList (listscanl f a as)

  -- -| 'scanr1' is a variant of 'scanr' that has no starting value argument.
  scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
  scanr1 f (a :| as) = fromList (listscanr1 f (a:as))

  -- -| 'intersperse x xs' alternates elements of the list with copies of @x@.
  --
  -- > intersperse 0 (1 :| [2,3]) == 1 :| [0,2,0,3]
  intersperse :: a -> NonEmpty a -> NonEmpty a
  intersperse a (b :| bs) = b :| case bs of
      [] -> []
      _:_ -> a : listintersperse a bs

  {-
  -- | @'iterate' f x@ produces the infinite sequence
  -- of repeated applications of @f@ to @x@.
  --
  -- > iterate f x = x :| [f x, f (f x), ..]
  iterate :: (a -> a) -> a -> NonEmpty a
  iterate f a = a :| listiterate f (f a)

  -- | @'cycle' xs@ returns the infinite repetition of @xs@:
  --
  -- > cycle (1 :| [2,3]) = 1 :| [2,3,1,2,3,...]
  cycle :: NonEmpty a -> NonEmpty a
  cycle = fromList . listcycle . toList
  -}

  -- -| 'reverse' a finite NonEmpty stream.
  reverse :: NonEmpty a -> NonEmpty a
  reverse = lift listreverse

  {-
  -- | @'repeat' x@ returns a constant stream, where all elements are
  -- equal to @x@.
  repeat :: a -> NonEmpty a
  repeat a = a :| listrepeat a
  -}

  -- -| @'take' n xs@ returns the first @n@ elements of @xs@.
  take :: Nat -> NonEmpty a -> [a]
  take n = listtake n . toList

  -- -| @'drop' n xs@ drops the first @n@ elements off the front of
  -- the sequence @xs@.
  drop :: Nat -> NonEmpty a -> [a]
  drop n = listdrop n . toList

  -- -| @'splitAt' n xs@ returns a pair consisting of the prefix of @xs@
  -- of length @n@ and the remaining stream immediately following this prefix.
  --
  -- > 'splitAt' n xs == ('take' n xs, 'drop' n xs)
  -- > xs == ys ++ zs where (ys, zs) = 'splitAt' n xs
  splitAt :: Nat -> NonEmpty a -> ([a],[a])
  splitAt n = listsplitAt n . toList

  -- -| @'takeWhile' p xs@ returns the longest prefix of the stream
  -- @xs@ for which the predicate @p@ holds.
  takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
  takeWhile p = listtakeWhile p . toList

  -- -| @'dropWhile' p xs@ returns the suffix remaining after
  -- @'takeWhile' p xs@.
  dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
  dropWhile p = listdropWhile p . toList

  -- -| @'span' p xs@ returns the longest prefix of @xs@ that satisfies
  -- @p@, together with the remainder of the stream.
  --
  -- > 'span' p xs == ('takeWhile' p xs, 'dropWhile' p xs)
  -- > xs == ys ++ zs where (ys, zs) = 'span' p xs
  span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
  span p = listspan p . toList

  -- -| The @'break' p@ function is equivalent to @'span' (not . p)@.
  break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
  break p = span (not . p)

  -- -| @'filter' p xs@ removes any elements from @xs@ that do not satisfy @p@.
  filter :: (a -> Bool) -> NonEmpty a -> [a]
  filter p = listfilter p . toList

  -- -| The 'partition' function takes a predicate @p@ and a stream
  -- @xs@, and returns a pair of lists. The first list corresponds to the
  -- elements of @xs@ for which @p@ holds; the second corresponds to the
  -- elements of @xs@ for which @p@ does not hold.
  --
  -- > 'partition' p xs = ('filter' p xs, 'filter' (not . p) xs)
  partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
  partition p = listpartition p . toList

  -- -| The 'group' function takes a stream and returns a list of
  -- streams such that flattening the resulting list is equal to the
  -- argument.  Moreover, each stream in the resulting list
  -- contains only equal elements.  For example, in list notation:
  --
  -- > 'group' $ 'cycle' "Mississippi"
  -- >   = "M" : "i" : "ss" : "i" : "ss" : "i" : "pp" : "i" : "M" : "i" : ...
  group :: Eq a => [a] -> [NonEmpty a]
  group = groupBy (==)

  -- -| 'groupBy' operates like 'group', but uses the provided equality
  -- predicate instead of `==`.
  groupBy :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
  groupBy eq0 = go eq0
    where
      go _  [] = []
      go eq (x : xs) = (x :| ys) : groupBy eq zs
        where (ys, zs) = listspan (eq x) xs

  -- -| 'groupWith' operates like 'group', but uses the provided projection when
  -- comparing for equality
  groupWith :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
  groupWith f = groupBy ((==) `on` f)

  -- -| 'groupAllWith' operates like 'groupWith', but sorts the list
  -- first so that each equivalence class has, at most, one list in the
  -- output
  groupAllWith :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
  groupAllWith f = groupWith f . listsortBy (compare `on` f)

  -- -| 'group1' operates like 'group', but uses the knowledge that its
  -- input is non-empty to produce guaranteed non-empty output.
  group1 :: Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
  group1 = groupBy1 (==)

  -- -| 'groupBy1' is to 'group1' as 'groupBy' is to 'group'.
  groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
  groupBy1 eq (x :| xs) = (x :| ys) :| groupBy eq zs
    where (ys, zs) = listspan (eq x) xs

  -- -| 'groupWith1' is to 'group1' as 'groupWith' is to 'group'
  groupWith1 :: (Eq b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
  groupWith1 f = groupBy1 ((==) `on` f)

  -- -| 'groupAllWith1' is to 'groupWith1' as 'groupAllWith' is to 'groupWith'
  groupAllWith1 :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
  groupAllWith1 f = groupWith1 f . sortWith f

  -- -| The 'isPrefix' function returns @True@ if the first argument is
  -- a prefix of the second.
  isPrefixOf :: Eq a => [a] -> NonEmpty a -> Bool
  isPrefixOf [] _ = True
  isPrefixOf (y:ys) (x :| xs) = (y == x) && listisPrefixOf ys xs

  -- -| @xs !! n@ returns the element of the stream @xs@ at index
  -- @n@. Note that the head of the stream has index 0.
  --
  -- /Beware/: a negative or out-of-bounds index will cause an error.
  (!!) :: NonEmpty a -> Nat -> a
  (!!) (x :| xs) n
    | n == 0 = x
    | n > 0  = xs `listindex` (n - 1)
    | otherwise = error "NonEmpty.!! negative argument"

  -- -| The 'zip' function takes two streams and returns a stream of
  -- corresponding pairs.
  zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a,b)
  zip (x :| xs) (y :| ys) = (x, y) :| listzip xs ys

  -- -| The 'zipWith' function generalizes 'zip'. Rather than tupling
  -- the elements, the elements are combined using the function
  -- passed as the first argument.
  zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
  zipWith f (x :| xs) (y :| ys) = f x y :| listzipWith f xs ys

  -- -| The 'unzip' function is the inverse of the 'zip' function.
  unzip :: NonEmpty (a,b) -> (NonEmpty a, NonEmpty b)
  unzip ((a,b) :| asbs) = (a :| as, b :| bs)
    where
      (as, bs) = listunzip asbs

  -- -| The 'nub' function removes duplicate elements from a list. In
  -- particular, it keeps only the first occurence of each element.
  -- (The name 'nub' means \'essence\'.)
  -- It is a special case of 'nubBy', which allows the programmer to
  -- supply their own inequality test.
  nub :: Eq a => NonEmpty a -> NonEmpty a
  nub = nubBy (==)

  -- -| The 'nubBy' function behaves just like 'nub', except it uses a
  -- user-supplied equality predicate instead of the overloaded '=='
  -- function.
  nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
  nubBy eq (a :| as) = a :| listnubBy eq (listfilter (\b -> not (eq a b)) as)

  -- -| 'transpose' for 'NonEmpty', behaves the same as 'Data.List.transpose'
  -- The rows/columns need not be the same length, in which case
  -- > transpose . transpose /= id
  transpose :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
  transpose = fmap fromList
            . fromList . listtranspose . toList
            . fmap toList

  -- -| 'sortBy' for 'NonEmpty', behaves the same as 'Data.List.sortBy'
  sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
  sortBy f = lift (listsortBy f)

  -- -| 'sortWith' for 'NonEmpty', behaves the same as:
  --
  -- > sortBy . comparing
  sortWith :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a
  sortWith = sortBy . comparing

  |])
