{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, TypeFamilies, TypeOperators,
             GADTs, ScopedTypeVariables, DeriveDataTypeable, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Bool
-- Copyright   :  (C) 2013-2014 Richard Eisenberg, Jan Stolarek
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

module Data.Singletons.Prelude.Bool (
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
  bool_, Bool_, sBool_, Otherwise, sOtherwise,

  -- * Defunctionalization symbols
  TrueSym0, FalseSym0,

  NotSym0, NotSym1,
  (:&&$), (:&&$$), (:&&$$$),
  (:||$), (:||$$), (:||$$$),
  Bool_Sym0, Bool_Sym1, Bool_Sym2, Bool_Sym3,
  OtherwiseSym0
  ) where

import Data.Singletons
import Data.Singletons.Prelude.Instances
import Data.Singletons.Single
import Data.Type.Bool ( If )

$(singletons [d|
  bool_ :: a -> a -> Bool -> a
  bool_ fls _tru False = fls
  bool_ _fls tru True  = tru
 |])

$(singletonsOnly [d|
  (&&) :: Bool -> Bool -> Bool
  False && _ = False
  True  && x = x
  infixr 3 &&

  (||) :: Bool -> Bool -> Bool
  False || x = x
  True  || _ = True
  infixr 2 ||

  not :: Bool -> Bool
  not False = True
  not True = False

  otherwise               :: Bool
  otherwise               =  True
  |])

-- | Conditional over singletons
sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c
