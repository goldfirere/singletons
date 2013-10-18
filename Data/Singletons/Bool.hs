{- Data/Singletons/Bool.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Defines functions and datatypes relating to the singleton for Bool.
-}

{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, TypeFamilies, TypeOperators,
             GADTs #-}

module Data.Singletons.Bool (
  If, sIf,
  Not, sNot, (:&&), (:&&:), (:||), (:||:), (%:&&), (%:||),
  Bool_, sBool_, Otherwise, sOtherwise,
  Sing(SFalse, STrue), SBool
  ) where

import Data.Singletons.Core
import Data.Singletons.Singletons

-- type-level conditional
type family If (a :: Bool) (b :: k) (c :: k) :: k
type instance If 'True b c = b
type instance If 'False b c = c

-- singleton conditional
sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c

-- ... with some functions over Booleans
$(singletonsOnly [d|
  not :: Bool -> Bool
  not False = True
  not True  = False

  (&&) :: Bool -> Bool -> Bool
  False && _ = False
  True  && a = a

  (||) :: Bool -> Bool -> Bool
  False || a = a
  True  || _ = True

  bool_ :: a -> a -> Bool -> a
  bool_ fls _tru False = fls
  bool_ _fls tru True  = tru

  otherwise :: Bool
  otherwise = True
  |])

-- symmetric syntax synonyms
type a :&&: b = a :&& b
type a :||: b = a :|| b
