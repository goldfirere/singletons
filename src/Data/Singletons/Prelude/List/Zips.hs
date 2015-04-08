{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             TemplateHaskell, GADTs, UndecidableInstances, RankNTypes,
             ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -O0 #-}
module Data.Singletons.Prelude.List.Zips where

import Data.Singletons.Single
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Instances

$(singletonsOnly [d|
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
 |])
