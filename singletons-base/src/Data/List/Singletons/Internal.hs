{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O0 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Singletons.Internal
-- Copyright   :  (C) 2013-2014 Richard Eisenberg, Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for '[]',
-- including singled versions of a few of the definitions in @Data.List@.
--
-- All of the functions defined in this module are specialized for lists,
-- unlike "Data.List.Singletons", which uses 'Foldable' and
-- 'Traversable' contexts when available.
--
----------------------------------------------------------------------------

module Data.List.Singletons.Internal where

import Control.Monad.Singletons.Internal
import Data.Bool.Singletons
import Data.Eq.Singletons
import Data.Maybe
import Data.Maybe.Singletons
import Data.Ord.Singletons
import Data.Semigroup.Singletons.Internal.Classes (SSemigroup(..), type (<>@#@$))
import Data.Semigroup.Singletons.Internal.Wrappers ()
import Data.Singletons.Base.Instances
import Data.Singletons.TH
import Data.Tuple.Singletons
import GHC.Base.Singletons
import GHC.Num.Singletons
import GHC.TypeLits.Singletons

$(singletonsOnly [d|
  head :: [a] -> a
  head (a : _) = a
  head []      = error "Data.Singletons.List.head: empty list"

  last :: [a] -> a
  last []       =  error "Data.Singletons.List.last: empty list"
  last [x]      =  x
  last (_:x:xs) =  last (x:xs)

  tail :: [a] -> [a]
  tail (_ : t) = t
  tail []      = error "Data.Singletons.List.tail: empty list"

  init                    :: [a] -> [a]
  init []                 =  error "Data.Singletons.List.init: empty list"
  init (x:xs)             =  init' x xs
     where init' :: a -> [a] -> [a]
           init' _ []     = []
           init' y (z:zs) = y : init' z zs

  null                    :: [a] -> Bool
  null []                 =  True
  null (_:_)              =  False

  reverse                 :: [a] -> [a]
  reverse l =  rev l []
    where
      rev :: [a] -> [a] -> [a]
      rev []     a = a
      rev (x:xs) a = rev xs (x:a)

  intersperse             :: a -> [a] -> [a]
  intersperse _   []      = []
  intersperse sep (x:xs)  = x : prependToAll sep xs

  intercalate :: [a] -> [[a]] -> [a]
  intercalate xs xss = concat (intersperse xs xss)

  subsequences            :: [a] -> [[a]]
  subsequences xs         =  [] : nonEmptySubsequences xs

  nonEmptySubsequences         :: [a] -> [[a]]
  nonEmptySubsequences []      =  []
  nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
    where f ys r = ys : (x : ys) : r

  prependToAll            :: a -> [a] -> [a]
  prependToAll _   []     = []
  prependToAll sep (x:xs) = sep : x : prependToAll sep xs

  permutations            :: forall a. [a] -> [[a]]
  permutations xs0        =  xs0 : perms xs0 []
    where
      perms []     _  = []
      perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
        where interleave    xs     r = let (_,zs) = interleave' id xs r in zs

              interleave' _ []     r = (ts, r)
              interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                       in  (y:us, f (t:y:us) : zs)

  foldl'           :: forall a b. (b -> a -> b) -> b -> [a] -> b
  foldl' f z0 xs0 = lgo z0 xs0
      where lgo :: b -> [a] -> b
            lgo z []     = z
            lgo z (x:xs) = let z' = f z x in z' `seq` lgo z' xs

  foldl1                  :: (a -> a -> a) -> [a] -> a
  foldl1 f (x:xs)         =  foldl f x xs
  foldl1 _ []             =  error "Data.Singletons.List.foldl1: empty list"

  foldl1'                  :: (a -> a -> a) -> [a] -> a
  foldl1' f (x:xs)         =  foldl' f x xs
  foldl1' _ []             =  error "Data.Singletons.List.foldl1': empty list"

  foldr1                  :: (a -> a -> a) -> [a] -> a
  foldr1 _ [x]            =  x
  foldr1 f (x:xs@(_:_))   =  f x (foldr1 f xs)
  foldr1 _ []             =  error "Data.Singletons.List.foldr1: empty list"

  concat :: [[a]] -> [a]
  concat = foldr (++) []

  concatMap               :: (a -> [b]) -> [a] -> [b]
  concatMap f             =  foldr ((++) . f) []

  and                     :: [Bool] -> Bool
  and []                  =  True
  and (x:xs)              =  x && and xs

  or                      :: [Bool] -> Bool
  or []                   =  False
  or (x:xs)               =  x || or xs

  all                     :: (a -> Bool) -> [a] -> Bool
  all _ []                =  True
  all p (x:xs)            =  p x && all p xs

  any                     :: (a -> Bool) -> [a] -> Bool
  any _ []                = False
  any p (x:xs)            = p x || any p xs

  scanl         :: (b -> a -> b) -> b -> [a] -> [b]
  scanl f q ls  =  q : (case ls of
                        []   -> []
                        x:xs -> scanl f (f q x) xs)
  scanl1                  :: (a -> a -> a) -> [a] -> [a]
  scanl1 f (x:xs)         =  scanl f x xs
  scanl1 _ []             =  []

  scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
  scanr _ q0 []           =  [q0]
  scanr f q0 (x:xs)       =  case scanr f q0 xs of
                               []     -> error "Data.Singletons.List.scanr: empty list"
                               (q:qs) -> f x q : (q:qs)

  scanr1                  :: (a -> a -> a) -> [a] -> [a]
  scanr1 _ []             =  []
  scanr1 _ [x]            =  [x]
  scanr1 f (x:xs@(_:_))   =  case scanr1 f xs of
                               []     -> error "Data.Singletons.List.scanr1: empty list"
                               (q:qs) -> f x q : (q:qs)

  mapAccumL :: (acc -> x -> (acc, y))
            -> acc
            -> [x]
            -> (acc, [y])
  mapAccumL _ s []        =  (s, [])
  mapAccumL f s (x:xs)    =  (s'',y:ys)
                             where (s', y ) = f s x
                                   (s'',ys) = mapAccumL f s' xs

  mapAccumR :: (acc -> x -> (acc, y))
              -> acc
              -> [x]
              -> (acc, [y])
  mapAccumR _ s []        =  (s, [])
  mapAccumR f s (x:xs)    =  (s'', y:ys)
                             where (s'',y ) = f s' x
                                   (s', ys) = mapAccumR f s xs

  unfoldr      :: (b -> Maybe (a, b)) -> b -> [a]
  unfoldr f b  =
    case f b of
     Just (a,new_b) -> a : unfoldr f new_b
     Nothing        -> []

  inits                   :: [a] -> [[a]]
  inits xs                =  [] : case xs of
                                    []      -> []
                                    x : xs' -> map (x :) (inits xs')

  tails                   :: [a] -> [[a]]
  tails xs                =  xs : case xs of
                                    []      -> []
                                    _ : xs' -> tails xs'

  isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
  isPrefixOf [] []        =  True
  isPrefixOf [] (_:_)     =  True
  isPrefixOf (_:_) []     =  False
  isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

  isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
  isSuffixOf x y          =  reverse x `isPrefixOf` reverse y

  isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
  isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

  elem                    :: (Eq a) => a -> [a] -> Bool
  elem _ []               = False
  elem x (y:ys)           = x==y || elem x ys
  infix 4 `elem`

  notElem                 :: (Eq a) => a -> [a] -> Bool
  notElem _ []            =  True
  notElem x (y:ys)        =  x /= y && notElem x ys
  infix 4 `notElem`

  zip :: [a] -> [b] -> [(a,b)]
  zip (x:xs) (y:ys) = (x,y) : zip xs ys
  zip [] []         = []
  zip (_:_) []      = []
  zip [] (_:_)      = []

  zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
  zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
  zip3 []     []     []     = []
  zip3 []     []     (_:_)  = []
  zip3 []     (_:_)     []  = []
  zip3 []     (_:_)  (_:_)  = []
  zip3 (_:_)  []     []     = []
  zip3 (_:_)  []     (_:_)  = []
  zip3 (_:_)  (_:_)  []     = []

  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
  zipWith _ [] []         = []
  zipWith _ (_:_) []      = []
  zipWith _ [] (_:_)      = []

  zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
  zipWith3 z (a:as) (b:bs) (c:cs) =  z a b c : zipWith3 z as bs cs
  zipWith3 _ []     []     []     = []
  zipWith3 _ []     []     (_:_)  = []
  zipWith3 _ []     (_:_)     []  = []
  zipWith3 _ []     (_:_)  (_:_)  = []
  zipWith3 _ (_:_)  []     []     = []
  zipWith3 _ (_:_)  []     (_:_)  = []
  zipWith3 _ (_:_)  (_:_)  []     = []

  unzip    :: [(a,b)] -> ([a],[b])
  unzip xs =  foldr (\(a,b) (as,bs) -> (a:as,b:bs)) ([],[]) xs

  -- Lazy patterns removed from unzip
  unzip3                  :: [(a,b,c)] -> ([a],[b],[c])
  unzip3 xs               =  foldr (\(a,b,c) (as,bs,cs) -> (a:as,b:bs,c:cs))
                                   ([],[],[]) xs

  unzip4                  :: [(a,b,c,d)] -> ([a],[b],[c],[d])
  unzip4 xs               =  foldr (\(a,b,c,d) (as,bs,cs,ds) ->
                                          (a:as,b:bs,c:cs,d:ds))
                                   ([],[],[],[]) xs

  unzip5                  :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
  unzip5 xs               =  foldr (\(a,b,c,d,e) (as,bs,cs,ds,es) ->
                                          (a:as,b:bs,c:cs,d:ds,e:es))
                                   ([],[],[],[],[]) xs

  unzip6                  :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
  unzip6 xs               =  foldr (\(a,b,c,d,e,f) (as,bs,cs,ds,es,fs) ->
                                          (a:as,b:bs,c:cs,d:ds,e:es,f:fs))
                                   ([],[],[],[],[],[]) xs

  unzip7                  :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
  unzip7 xs               =  foldr (\(a,b,c,d,e,f,g) (as,bs,cs,ds,es,fs,gs) ->
                                          (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
                                   ([],[],[],[],[],[],[]) xs

-- We can't promote any of these functions because at the type level
-- String literals are no longer considered to be lists of Chars, so
-- there is mismatch between term-level and type-level semantics
--  lines                   :: String -> [String]
--  lines ""                =  []
--  lines s                 =  cons (case break (== '\n') s of
--                                      (l, s') -> (l, case s' of
--                                                      []      -> []
--                                                      _:s''   -> lines s''))
--      where
--        cons ~(h, t)        =  h : t
--
--  words                   :: String -> [String]
--  words s                 =  case dropWhile isSpace s of
--                                  "" -> []
--                                  s' -> w : words s''
--                                        where (w, s'') =
--                                               break isSpace s'

  unlines                 :: [Symbol] -> Symbol
  unlines []              = ""
  unlines (l:ls)          = l <> "\n" <> unlines ls

  unwords                 :: [Symbol] -> Symbol
  unwords []              = ""
  unwords (w:ws)          = w <> go ws
    where
      go []     = ""
      go (v:vs) = " " <> (v <> go vs)

  delete                  :: (Eq a) => a -> [a] -> [a]
  delete                  =  deleteBy (==)

  (\\)                    :: (Eq a) => [a] -> [a] -> [a]
  (\\)                    =  foldl (flip delete)
  infix 5 \\      -- This comment is necessary so CPP doesn't treat the
                  -- trailing backslash as a line splice. Urgh.

  deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
  deleteBy _  _ []        = []
  deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

  deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

  sortBy :: (a -> a -> Ordering) -> [a] -> [a]
  sortBy cmp  = foldr (insertBy cmp) []

  insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
  insertBy _   x [] = [x]
  insertBy cmp x ys@(y:ys')
   = case cmp x y of
       GT -> y : insertBy cmp x ys'
       LT  -> x : ys
       EQ  -> x : ys

  maximumBy               :: (a -> a -> Ordering) -> [a] -> a
  maximumBy _ []          =  error "Data.Singletons.List.maximumBy: empty list"
  maximumBy cmp xs@(_:_)  =  foldl1 maxBy xs
                          where
                            maxBy x y = case cmp x y of
                                         GT -> x
                                         EQ -> y
                                         LT -> y

  minimumBy               :: (a -> a -> Ordering) -> [a] -> a
  minimumBy _ []          =  error "Data.Singletons.List.minimumBy: empty list"
  minimumBy cmp xs@(_:_)  =  foldl1 minBy xs
                          where
                            minBy x y = case cmp x y of
                                         GT -> y
                                         EQ -> x
                                         LT -> x

  filter :: (a -> Bool) -> [a] -> [a]
  filter _p []    = []
  filter p  (x:xs) = if p x then x : filter p xs else filter p xs

  find                    :: (a -> Bool) -> [a] -> Maybe a
  find p                  = listToMaybe . filter p

-- These three rely on findIndices, which does not promote.
-- Since we have our own implementation of findIndices these are perfectly valid
  elemIndex       :: Eq a => a -> [a] -> Maybe Natural
  elemIndex x     = findIndex (x==)

  elemIndices     :: Eq a => a -> [a] -> [Natural]
  elemIndices x   = findIndices (x==)

  findIndex       :: (a -> Bool) -> [a] -> Maybe Natural
  findIndex p     = listToMaybe . findIndices p

-- Uses infinite lists and and Ints
--  findIndices      :: (a -> Bool) -> [a] -> [Int]
--  findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]

  findIndices      :: (a -> Bool) -> [a] -> [Natural]
  findIndices p xs = map snd (filter (\(x,_) -> p x)
                                     (zip xs (buildList 0 xs)))
    where buildList :: Natural -> [b] -> [Natural]
          buildList _ []     = []
          buildList a (_:rest) = a : buildList (a+1) rest

  intersect               :: (Eq a) => [a] -> [a] -> [a]
  intersect               =  intersectBy (==)

  intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  intersectBy _  []       []       =  []
  intersectBy _  []       (_:_)    =  []
  intersectBy _  (_:_)    []       =  []
  intersectBy eq xs@(_:_) ys@(_:_) =  [x | x <- xs, any (eq x) ys]

  takeWhile               :: (a -> Bool) -> [a] -> [a]
  takeWhile _ []          =  []
  takeWhile p (x:xs)      = if p x then x : takeWhile p xs else []

  dropWhile               :: (a -> Bool) -> [a] -> [a]
  dropWhile _ []          =  []
  dropWhile p xs@(x:xs')  = if p x then dropWhile p xs' else xs

  dropWhileEnd            :: (a -> Bool) -> [a] -> [a]
  dropWhileEnd p          = foldr (\x xs -> if p x && null xs then [] else x : xs) []

  span                    :: (a -> Bool) -> [a] -> ([a],[a])
  span _ xs@[]            =  (xs, xs)
  span p xs@(x:xs')       = if p x then let (ys,zs) = span p xs' in (x:ys,zs)
                                   else ([], xs)

  break                   :: (a -> Bool) -> [a] -> ([a],[a])
  break _ xs@[]           =  (xs, xs)
  break p xs@(x:xs')      = if p x then ([],xs)
                                   else let (ys,zs) = break p xs' in (x:ys,zs)

-- Can't be promoted because of limitations of Int promotion
-- Below is a re-implementation using Nat
--  take                   :: Int -> [a] -> [a]
--  take n _      | n <= 0 =  []
--  take _ []              =  []
--  take n (x:xs)          =  x : take (n-1) xs

--  drop                   :: Int -> [a] -> [a]
--  drop n xs     | n <= 0 =  xs
--  drop _ []              =  []
--  drop n (_:xs)          =  drop (n-1) xs

--  splitAt                :: Int -> [a] -> ([a],[a])
--  splitAt n xs           =  (take n xs, drop n xs)

  take                   :: Natural -> [a] -> [a]
  take _ []              =  []
  take n (x:xs)          = if n == 0 then [] else x : take (n-1) xs

  drop                   :: Natural -> [a] -> [a]
  drop _ []              = []
  drop n (x:xs)          = if n == 0 then x:xs else drop (n-1) xs

  splitAt                :: Natural -> [a] -> ([a],[a])
  splitAt n xs           =  (take n xs, drop n xs)

  group                   :: Eq a => [a] -> [[a]]
  group xs                =  groupBy (==) xs

  maximum                 :: (Ord a) => [a] -> a
  maximum []              =  error "Data.Singletons.List.maximum: empty list"
  maximum xs@(_:_)        =  foldl1 max xs

  minimum                 :: (Ord a) => [a] -> a
  minimum []              =  error "Data.Singletons.List.minimum: empty list"
  minimum xs@(_:_)        =  foldl1 min xs

  insert :: Ord a => a -> [a] -> [a]
  insert e ls = insertBy (compare) e ls

  sort :: (Ord a) => [a] -> [a]
  sort = sortBy compare

  groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
  groupBy _  []           =  []
  groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                             where (ys,zs) = span (eq x) xs

  lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
  lookup _key []          =  Nothing
  lookup  key ((x,y):xys) = if key == x then Just y else lookup key xys

  partition               :: (a -> Bool) -> [a] -> ([a],[a])
  partition p xs          = foldr (select p) ([],[]) xs

  -- Lazy pattern removed from select
  select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
  select p x (ts,fs) = if p x then (x:ts,fs) else (ts, x:fs)

-- Can't be promoted because of limitations of Int promotion
-- Below is a re-implementation using Nat
--  sum                     :: (Num a) => [a] -> a
--  sum     l       = sum' l 0
--    where
--      sum' []     a = a
--      sum' (x:xs) a = sum' xs (a+x)
--
--  product                 :: (Num a) => [a] -> a
--  product l       = prod l 1
--    where
--      prod []     a = a
--      prod (x:xs) a = prod xs (a*x)

  sum                     :: forall a. Num a => [a] -> a
  sum     l       = sum' l 0
    where
      sum' :: [a] -> a -> a
      sum' []     a = a
      sum' (x:xs) a = sum' xs (a+x)

  product                 :: forall a. Num a => [a] -> a
  product l       = prod l 1
    where
      prod :: [a] -> a -> a
      prod []     a = a
      prod (x:xs) a = prod xs (a*x)


-- Can't be promoted because of limitations of Int promotion
-- Below is a re-implementation using Nat
--  length                  :: [a] -> Int
--  length l                =  lenAcc l 0#
--
--  lenAcc :: [a] -> Int# -> Int
--  lenAcc []     a# = I# a#
--  lenAcc (_:xs) a# = lenAcc xs (a# +# 1#)
--
--  incLen :: a -> (Int# -> Int) -> Int# -> Int
--  incLen _ g x = g (x +# 1#)

  length :: [a] -> Natural
  length []     = 0
  length (_:xs) = 1 + length xs

-- Functions working on infinite lists don't promote because they create
-- infinite types. replicate also uses integers, but luckily it can be rewritten
--  iterate :: (a -> a) -> a -> [a]
--  iterate f x =  x : iterate f (f x)
--
--  repeat :: a -> [a]
--  repeat x = xs where xs = x : xs
--
--  replicate               :: Int -> a -> [a]
--  replicate n x           =  take n (repeat x)
--
--  cycle                   :: [a] -> [a]
--  cycle []                = error "Data.Singletons.List.cycle: empty list"
--  cycle xs                = xs' where xs' = xs ++ xs'

  replicate               :: Natural -> a -> [a]
  replicate n x           = if n == 0 then [] else x : replicate (n-1) x

-- Uses partial pattern-matching in a list comprehension
-- (see https://github.com/goldfirere/singletons/issues/340)
--  transpose               :: [[a]] -> [[a]]
--  transpose []             = []
--  transpose ([]   : xss)   = transpose xss
--  transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

  transpose               :: [[a]] -> [[a]]
  transpose []             = []
  transpose ([]   : xss)   = transpose xss
  transpose ((x:xs) : xss) = (x : (map head xss)) : transpose (xs : (map tail xss))

-- Can't be promoted because of limitations of Int promotion.
-- Below is a re-implementation using Nat
--  (!!)                    :: [a] -> Int -> a
--  xs     !! n | n < 0 =  error "Data.Singletons.List.!!: negative index"
--  []     !! _         =  error "Data.Singletons.List.!!: index too large"
--  (x:_)  !! 0         =  x
--  (_:xs) !! n         =  xs !! (n-1)

  (!!)                    :: [a] -> Natural -> a
  []     !! _         =  error "Data.Singletons.List.!!: index too large"
  (x:xs) !! n         =  if n == 0 then x else xs !! (n-1)
  infixl 9 !!

  nub                     :: forall a. (Eq a) => [a] -> [a]
  nub l                   = nub' l []
    where
      nub' :: [a] -> [a] -> [a]
      nub' [] _           = []
      nub' (x:xs) ls      = if x `elem` ls then nub' xs ls else x : nub' xs (x:ls)

  nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
  nubBy eq l              = nubBy' l []
    where
      nubBy' [] _         = []
      nubBy' (y:ys) xs    = if elem_by eq y xs then nubBy' ys xs else y : nubBy' ys (y:xs)

  elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
  elem_by _  _ []         =  False
  elem_by eq y (x:xs)     =  y `eq` x || elem_by eq y xs

  unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

  union                   :: (Eq a) => [a] -> [a] -> [a]
  union                   = unionBy (==)

  genericLength :: (Num i) => [a] -> i
  genericLength []     = 0
  genericLength (_:xs) = 1 + genericLength xs

  |])

-- The following functions are supported for promotion only.
$(promoteOnly [d|

  -- Overlapping patterns don't singletonize
  stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
  stripPrefix [] ys = Just ys
  stripPrefix (x:xs) (y:ys)
   | x == y = stripPrefix xs ys
  stripPrefix _ _ = Nothing

  -- To singletonize these we would need to rewrite all patterns
  -- as non-overlapping. This means 2^7 equations for zipWith7.

  zip4                    :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
  zip4                    =  zipWith4 (,,,)

  zip5                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
  zip5                    =  zipWith5 (,,,,)

  zip6                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [(a,b,c,d,e,f)]
  zip6                    =  zipWith6 (,,,,,)

  zip7                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [g] -> [(a,b,c,d,e,f,g)]
  zip7                    =  zipWith7 (,,,,,,)

  zipWith4                :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
  zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
                          =  z a b c d : zipWith4 z as bs cs ds
  zipWith4 _ _ _ _ _      =  []

  zipWith5                :: (a->b->c->d->e->f) ->
                             [a]->[b]->[c]->[d]->[e]->[f]
  zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
                          =  z a b c d e : zipWith5 z as bs cs ds es
  zipWith5 _ _ _ _ _ _    = []

  zipWith6                :: (a->b->c->d->e->f->g) ->
                             [a]->[b]->[c]->[d]->[e]->[f]->[g]
  zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
                          =  z a b c d e f : zipWith6 z as bs cs ds es fs
  zipWith6 _ _ _ _ _ _ _  = []

  zipWith7                :: (a->b->c->d->e->f->g->h) ->
                             [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
  zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
                     =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
  zipWith7 _ _ _ _ _ _ _ _ = []
 |])
