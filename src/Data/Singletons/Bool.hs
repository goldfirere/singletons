{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, TypeFamilies, TypeOperators,
             GADTs, CPP #-}

#if __GLASGOW_HASKELL__ < 707
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Bool
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for 'Bool',
-- including a singletons version of all the definitions in @Data.Bool@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Bool@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Singletons.Bool (
  -- * The 'Bool' singleton

  Sing(SFalse, STrue),
  -- | Though Haddock doesn't show it, the 'Sing' instance above declares
  -- constructors
  --
  -- > SFalse :: Sing False
  -- > STrue  :: Sing True

  SBool,
  -- | 'SBool' is a kind-restricted synonym for 'Sing': @type SBool (a :: Bool) = Sing a@

  -- * Conditionals
  If, sIf,

  -- * Singletons from @Data.Bool@
  Not, sNot, (:&&), (:||), (%:&&), (%:||),

  -- | The following are derived from the function 'bool' in @Data.Bool@. The extra
  -- underscore is to avoid name clashes with the type 'Bool'.
  Bool_, sBool_, Otherwise, sOtherwise,

  -- * Defunctionalization symbols
  TrueSym0, FalseSym0,

  NotSym0, (:&&$), (:&&$$), (:||$), (:||$$),
  Bool_Sym0, Bool_Sym1, Bool_Sym2, OtherwiseSym0
  ) where

import Data.Singletons.Exports
import Data.Singletons.Core
import Data.Singletons.Singletons
import Data.Singletons.Types

#if __GLASGOW_HASKELL__ >= 707
import Data.Type.Bool

type a :&& b = a && b
type a :|| b = a || b

(%:&&) :: SBool a -> SBool b -> SBool (a :&& b)
SFalse %:&& _ = SFalse
STrue  %:&& a = a

data (:&&$$) (a :: Bool) (b :: TyFun Bool Bool)
data (:&&$)  (a :: TyFun Bool (TyFun Bool Bool -> *))
type instance ((:&&$$) a) @@ b = a :&& b
type instance (:&&$)      @@ a = (:&&$$) a

(%:||) :: SBool a -> SBool b -> SBool (a :|| b)
SFalse %:|| a = a
STrue  %:|| _ = STrue

data (:||$$) (a :: Bool) (b :: TyFun Bool Bool)
data (:||$)  (a :: TyFun Bool (TyFun Bool Bool -> *))
type instance ((:||$$) a) @@ b = a :|| b
type instance (:||$)      @@ a = (:||$$) a

#else

$(singletonsOnly [d|
  not :: Bool -> Bool
  not False = True
  not True  = False

  (&&) :: Bool -> Bool -> Bool
  False && _ = False
  True  && x = x

  (||) :: Bool -> Bool -> Bool
  False || x = x
  True  || _ = True
  |])

#endif

sNot :: SBool a -> SBool (Not a)
sNot SFalse = STrue
sNot STrue  = SFalse

data NotSym0 (t :: TyFun Bool Bool)
type instance NotSym0 @@ a = Not a

-- | Conditional over singletons
sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c

-- ... with some functions over Booleans
$(singletonsOnly [d|
  bool_ :: a -> a -> Bool -> a
  bool_ fls _tru False = fls
  bool_ _fls tru True  = tru

  otherwise :: Bool
  otherwise = True
  |])
