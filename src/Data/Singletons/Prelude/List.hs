{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             TemplateHaskell, GADTs, UndecidableInstances, RankNTypes,
             ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -O0 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.List
-- Copyright   :  (C) 2013-2014 Richard Eisenberg, Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for '[]',
-- including a singletons version of a few of the definitions in @Data.List@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.List@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.List (
  -- * The singleton for lists
  Sing(SNil, SCons),
  -- | Though Haddock doesn't show it, the 'Sing' instance above declares
  -- constructors
  --
  -- > SNil  :: Sing '[]
  -- > SCons :: Sing (h :: k) -> Sing (t :: [k]) -> Sing (h ': t)

  SList,
  -- | 'SList' is a kind-restricted synonym for 'Sing': @type SList (a :: [k]) = Sing a@

  -- * Basic functions
  (:++), (%:++), Head, sHead, Last, sLast, Tail, sTail, Init, sInit,
  Null, sNull,

   -- * List transformations
  Map, sMap, Reverse, sReverse, Intersperse, sIntersperse,
  Intercalate, sIntercalate, Subsequences, sSubsequences,
  Permutations, sPermutations,

  -- * Reducing lists (folds)
  Foldl, sFoldl, Foldl', sFoldl', Foldl1, sFoldl1, Foldl1', sFoldl1',
  Foldr, sFoldr, Foldr1, sFoldr1,

  -- ** Special folds
  Concat, sConcat, ConcatMap, sConcatMap,
  And, sAnd, Or, sOr, Any_, sAny_, All, sAll,
  any_, -- equivalent of Data.List `any`. Avoids name clash with Any type

  -- * Building lists

  -- ** Scans
  Scanl, sScanl, Scanl1, sScanl1, Scanr, sScanr, Scanr1, sScanr1,

  -- ** Accumulating maps
  MapAccumL, sMapAccumL, MapAccumR, sMapAccumR,

  -- ** Unfolding
  Unfoldr, sUnfoldr,

  -- * Sublists

  -- ** Extracting sublists
  Inits, sInits, Tails, sTails,

  -- ** Predicates
  IsPrefixOf, sIsPrefixOf, IsSuffixOf, sIsSuffixOf, IsInfixOf, sIsInfixOf,

  -- * Searching lists

  -- ** Searching by equality
  Elem, sElem, NotElem, sNotElem,

  -- * Zipping and unzipping lists
  Zip, sZip, Zip3, sZip3, ZipWith, sZipWith, ZipWith3, sZipWith3,
  Unzip, sUnzip, Unzip3, sUnzip3, Unzip4, sUnzip4,
  Unzip5, sUnzip5, Unzip6, sUnzip6, Unzip7, sUnzip7,

  -- * Special lists

  -- ** \"Set\" operations
  Delete, sDelete, (:\\), (%:\\),

  -- ** Ordered lists
  -- Insert, sInsert, Sort, sSort,

  -- * Generalized functions

  -- ** The \"@By@\" operations
  DeleteBy, sDeleteBy, DeleteFirstsBy, sDeleteFirstsBy,

  SortBy, sSortBy, InsertBy, sInsertBy,
  MaximumBy, sMaximumBy, MinimumBy, sMinimumBy,

  -- * Defunctionalization symbols
  NilSym0,
  (:$), (:$$), (:$$$),

  (:++$$$), (:++$$), (:++$), HeadSym0, HeadSym1, LastSym0, LastSym1,
  TailSym0, TailSym1, InitSym0, InitSym1, NullSym0, NullSym1,

  MapSym0, MapSym1, MapSym2, ReverseSym0, ReverseSym1,
  IntersperseSym0, IntersperseSym1, IntersperseSym2,
  IntercalateSym0, IntercalateSym1, IntercalateSym2,
  SubsequencesSym0, SubsequencesSym1,
  PermutationsSym0, PermutationsSym1,

  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  Foldl'Sym0, Foldl'Sym1, Foldl'Sym2, Foldl'Sym3,
  Foldl1Sym0, Foldl1Sym1, Foldl1Sym2,
  Foldl1'Sym0, Foldl1'Sym1, Foldl1'Sym2,
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  Foldr1Sym0, Foldr1Sym1, Foldr1Sym2,

  ConcatSym0, ConcatSym1,
  ConcatMapSym0, ConcatMapSym1, ConcatMapSym2,
  AndSym0, AndSym1, OrSym0, OrSym1,
  Any_Sym0, Any_Sym1, Any_Sym2,
  AllSym0, AllSym1, AllSym2,

  ScanlSym0, ScanlSym1, ScanlSym2, ScanlSym3,
  Scanl1Sym0, Scanl1Sym1, Scanl1Sym2,
  ScanrSym0, ScanrSym1, ScanrSym2, ScanrSym3,
  Scanr1Sym0, Scanr1Sym1, Scanr1Sym2,

  MapAccumLSym0, MapAccumLSym1, MapAccumLSym2, MapAccumLSym3,
  MapAccumRSym0, MapAccumRSym1, MapAccumRSym2, MapAccumRSym3,

  UnfoldrSym0, UnfoldrSym1, UnfoldrSym2,

  InitsSym0, InitsSym1, TailsSym0, TailsSym1,

  IsPrefixOfSym0, IsPrefixOfSym1, IsPrefixOfSym2,
  IsSuffixOfSym0, IsSuffixOfSym1, IsSuffixOfSym2,
  IsInfixOfSym0, IsInfixOfSym1, IsInfixOfSym2,

  ElemSym0, ElemSym1, ElemSym2,
  NotElemSym0, NotElemSym1, NotElemSym2,

  ZipSym0, ZipSym1, ZipSym2,
  Zip3Sym0, Zip3Sym1, Zip3Sym2, Zip3Sym3,
  ZipWithSym0, ZipWithSym1, ZipWithSym2, ZipWithSym3,
  ZipWith3Sym0, ZipWith3Sym1, ZipWith3Sym2, ZipWith3Sym3, ZipWith3Sym4,
  UnzipSym0, UnzipSym1,
  Unzip3Sym0, Unzip3Sym1,
  Unzip4Sym0, Unzip4Sym1,
  Unzip5Sym0, Unzip5Sym1,
  Unzip6Sym0, Unzip6Sym1,
  Unzip7Sym0, Unzip7Sym1,

  DeleteSym0, DeleteSym1, DeleteSym2,
  (:\\$), (:\\$$), (:\\$$$),

  -- InsertSym0, InsertSym1, InsertSym2,
  -- SortSym0, SortSym1,

  DeleteBySym0, DeleteBySym1, DeleteBySym2, DeleteBySym3,
  DeleteFirstsBySym0, DeleteFirstsBySym1, DeleteFirstsBySym2, DeleteFirstsBySym3,

  SortBySym0, SortBySym1, SortBySym2,
  InsertBySym0, InsertBySym1, InsertBySym2, InsertBySym3,
  MaximumBySym0, MaximumBySym1, MaximumBySym2,
  MinimumBySym0, MinimumBySym1, MinimumBySym2,
  ) where

import Data.Singletons
import Data.Singletons.Prelude.Instances
import Data.Singletons.Single
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Eq

$(singletons [d|
  any_                     :: (a -> Bool) -> [a] -> Bool
  any_ _ []                = False
  any_ p (x:xs)            = p x || any_ p xs
 |])

$(singletonsOnly [d|
  head :: [a] -> a
  head (a : _) = a
  head []      = error "Data.Singletons.List.head: empty list"

  last :: [a] -> a
  last []      =  error "Data.Singletons.List.last: empty list"
  last (x:xs)  =  last' x xs
    where last' :: a -> [a] -> a
          last' y []     = y
          last' _ (y:ys) = last' y ys

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

  permutations            :: [a] -> [[a]]
  permutations xs0        =  xs0 : perms xs0 []
    where
      perms []     _  = []
      perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
        where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
              interleave' _ []     r = (ts, r)
              interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                       in  (y:us, f (t:y:us) : zs)

  foldl        :: (b -> a -> b) -> b -> [a] -> b
  foldl f z0 xs0 = lgo z0 xs0
               where
                 lgo :: b -> [a] -> b
                 lgo z []     =  z
                 lgo z (x:xs) = lgo (f z x) xs

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
  isInfixOf needle haystack = any_ (isPrefixOf needle) (tails haystack)

  elem                    :: (Eq a) => a -> [a] -> Bool
  elem _ []               = False
  elem x (y:ys)           = x==y || elem x ys

  notElem                 :: (Eq a) => a -> [a] -> Bool
  notElem _ []            =  True
  notElem x (y:ys)        =  x /= y && notElem x ys

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
--  unlines                 :: [String] -> String
--  unlines                 =  concatMap (++ "\n")
--
--  words                   :: String -> [String]
--  words s                 =  case dropWhile isSpace s of
--                                  "" -> []
--                                  s' -> w : words s''
--                                        where (w, s'') =
--                                               break isSpace s'
--
--  unwords                 :: [String] -> String
--  unwords []              =  ""
--  unwords ws              =  foldr1 (\w s -> w ++ ' ':s) ws

  delete                  :: (Eq a) => a -> [a] -> [a]
  delete                  =  deleteBy (==)

  (\\)                    :: (Eq a) => [a] -> [a] -> [a]
  (\\)                    =  foldl (flip delete)

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

  |])
