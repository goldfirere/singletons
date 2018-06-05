{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.ShowSing
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the class 'ShowSing', allowing for conversion of 'Sing' values to
-- readable 'String's.
--
----------------------------------------------------------------------------

module Data.Singletons.ShowSing (
  -- * The 'ShowSing' class
  ShowSing(..),
  ) where

import Data.Singletons.Internal
import Data.Singletons.Prelude.Instances
import Data.Singletons.Single
import Data.Singletons.TypeLits.Internal
import Data.Singletons.Util

import GHC.Show (appPrec, appPrec1)
import GHC.TypeLits (symbolVal)
import qualified GHC.TypeNats as TN (natVal)

----------------------------------------------------------------------
---- ShowSing --------------------------------------------------------
----------------------------------------------------------------------

-- | Members of the 'ShowSing' kind class can have their 'Sing' values
-- converted to 'String's in a fashion similar to that of the 'Show' class.
-- (In fact, this class only exists because one cannot write 'Show' instances
-- for 'Sing's of the form
-- @instance (forall z. Show (Sing (z :: k))) => Show (Sing (x :: [k]))@.)
--
-- This class should not be confused with the promoted or singled versions of
-- 'Show' from "Data.Singletons.Prelude.Show" (@PShow@ and @SShow@, respectively).
-- The output of 'ShowSing' is intended to reflect the singleton type, whereas
-- the output of @PShow@ and @SShow@ reflects the original type. That is, showing
-- @SFalse@ with 'ShowSing' would yield @\"SFalse\"@, whereas @PShow@ and @SShow@
-- would yield @\"False\"@.
--
-- Instances of this class are generated alongside singleton definitions for
-- datatypes that derive a 'Show' instance. Moreover, having a 'ShowSing'
-- instances makes it simple to define a 'Show' instance. For instance:
--
-- @
-- instance 'ShowSing' a => 'ShowSing' [a] where
--   'showsSingPrec' = ...
-- instance 'ShowSing' a => 'Show' ('Sing' (x :: [a])) where
--   'showsPrec' = 'showsSingPrec'
-- @
--
-- As a result, singleton definitions for datatypes that derive a 'Show'
-- instance also get a 'Show' instance for the singleton type as well
-- (in addition to promoted and singled 'Show' instances).
--
-- To recap: 'singletons' will give you all of these for a datatype that derives
-- a 'Show' instance:
--
-- * A promoted (@PShow@) instance
-- * A singled (@SShow@) instance
-- * A 'ShowSing' instance for the singleton type
-- * A 'Show' instance for the singleton type
--
-- What a bargain!
class ShowSing k where
  -- | @'showsSingPrec' p s@ convert a 'Sing' value @p@ to a readable 'String'
  -- with precedence @p@.
  showsSingPrec :: Int -> Sing (a :: k) -> ShowS

------------------------------------------------------------
-- TypeLits instances
------------------------------------------------------------

-- These are a bit special because the singleton constructor does not uniquely
-- determine the type being used in the constructor's return type (e.g., all Nats
-- have the same singleton constructor, SNat). To compensate for this, we display
-- the type being used using visible type application. (Thanks to @cumber on #179
-- for suggesting this implementation.)

instance ShowSing Nat where
  showsSingPrec p n@SNat
    = showParen (p > appPrec)
      ( showString "SNat @"
        . showsPrec appPrec1 (TN.natVal n)
      )
instance Show (SNat n) where
  showsPrec = showsSingPrec

instance ShowSing Symbol where
  showsSingPrec p s@SSym
    = showParen (p > appPrec)
      ( showString "SSym @"
        . showsPrec appPrec1 (symbolVal s)
      )
instance Show (SSymbol s) where
  showsPrec = showsSingPrec

------------------------------------------------------------
-- Template Haskell-generated instances
------------------------------------------------------------

$(showSingInstances basicTypes)
