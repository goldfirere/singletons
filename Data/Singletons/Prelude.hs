{- Data/Singletons/Prelude.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Mimics the Haskell Prelude, but with singleton types. Includes the basic
singleton definitions.
Note: This is currently very incomplete!
-}

module Data.Singletons.Prelude (
  module Data.Singletons,
  Sing(SFalse, STrue, SNil, SCons, SJust, SNothing, SLeft, SRight,
       STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7),
  SBool, SList, SMaybe, SEither,
  STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7,
  If, sIf, Not, sNot, (:&&), (:&&:), (:||), (:||:), (%:&&), (%:||),
  Head, Tail, (:++), (%:++),
  module Data.Singletons.Eq,
  Maybe_, sMaybe_,
  Either_, sEither_,
  Fst, sFst, Snd, sSnd, Curry, sCurry, Uncurry, sUncurry
  ) where

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.List
import Data.Singletons.Maybe
import Data.Singletons.Either
import Data.Singletons.Tuple
import Data.Singletons.Eq
