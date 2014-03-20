{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeFamilies,
             DataKinds, PolyKinds, UndecidableInstances, GADTs,
             RankNTypes, CPP #-}

#if __GLASGOW_HASKELL__ < 707
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Maybe
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for 'Maybe',
-- including a singletons version of all the definitions in @Data.Maybe@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Maybe@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------


module Data.Singletons.Maybe (
  -- The 'Maybe' singleton

  Sing(SNothing, SJust),
  -- | Though Haddock doesn't show it, the 'Sing' instance above declares
  -- constructors
  --
  -- > SNothing :: Sing Nothing
  -- > SJust    :: Sing a -> Sing (Just a)

  SMaybe,
  -- | 'SBool' is a kind-restricted synonym for 'Sing': @type SMaybe (a :: Maybe k) = Sing a@

  -- * Singletons from @Data.Maybe@

  Maybe_, sMaybe_,
  -- | The preceding two definitions are derived from the function 'maybe' in
  -- @Data.Maybe@. The extra underscore is to avoid name clashes with the type
  -- 'Maybe'.

  IsJust, sIsJust, IsNothing, sIsNothing,
  FromJust, sFromJust, FromMaybe, sFromMaybe, MaybeToList, sMaybeToList,
  ListToMaybe, sListToMaybe, CatMaybes, sCatMaybes, MapMaybe, sMapMaybe
  ) where

import Data.Singletons.Instances
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.List
import Data.Singletons.TypeLits

$(singletonsOnly [d|
  -- | The 'maybe' function takes a default value, a function, and a 'Maybe'
  -- value.  If the 'Maybe' value is 'Nothing', the function returns the
  -- default value.  Otherwise, it applies the function to the value inside
  -- the 'Just' and returns the result.
  maybe_ :: b -> (a -> b) -> Maybe a -> b
  maybe_ n _ Nothing  = n
  maybe_ _ f (Just x) = f x

  -- | The 'isJust' function returns 'True' iff its argument is of the
  -- form @Just _@.
  isJust         :: Maybe a -> Bool
  isJust Nothing  = False
  isJust (Just _) = True

  -- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
  isNothing         :: Maybe a -> Bool
  isNothing Nothing  = True
  isNothing (Just _) = False

  -- | The 'fromJust' function extracts the element out of a 'Just' and
  -- throws an error if its argument is 'Nothing'.
  fromJust          :: Maybe a -> a
  fromJust Nothing  = error "Maybe.fromJust: Nothing" -- yuck
  fromJust (Just x) = x

  -- | The 'fromMaybe' function takes a default value and and 'Maybe'
  -- value.  If the 'Maybe' is 'Nothing', it returns the default values;
  -- otherwise, it returns the value contained in the 'Maybe'.
  fromMaybe     :: a -> Maybe a -> a
  fromMaybe d Nothing  = d
  fromMaybe _ (Just v) = v

  -- | The 'maybeToList' function returns an empty list when given
  -- 'Nothing' or a singleton list when not given 'Nothing'.
  maybeToList            :: Maybe a -> [a]
  maybeToList  Nothing   = []
  maybeToList  (Just x)  = [x]

  -- | The 'listToMaybe' function returns 'Nothing' on an empty list
  -- or @'Just' a@ where @a@ is the first element of the list.
  listToMaybe           :: [a] -> Maybe a
  listToMaybe []        =  Nothing
  listToMaybe (a:_)     =  Just a

  -- | The 'catMaybes' function takes a list of 'Maybe's and returns
  -- a list of all the 'Just' values.
  catMaybes              :: [Maybe a] -> [a]
  catMaybes []             = []
  catMaybes (Just x  : xs) = x : catMaybes xs
  catMaybes (Nothing : xs) = catMaybes xs

  -- | The 'mapMaybe' function is a version of 'map' which can throw
  -- out elements.  In particular, the functional argument returns
  -- something of type @'Maybe' b@.  If this is 'Nothing', no element
  -- is added on to the result list.  If it just @'Just' b@, then @b@ is
  -- included in the result list.
  mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
  mapMaybe _ []     = []
  mapMaybe f (x:xs) = maybeToList (f x) ++ (mapMaybe f xs)
  |])
