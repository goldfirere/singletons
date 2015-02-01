{- Data/Singletons/Void.hs

   A reimplementation of a Void type, copied shamelessly from Edward Kmett's void
   package, but without inducing a dependency.

-}

{-# LANGUAGE Safe, DeriveDataTypeable, DeriveGeneric, StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2013 Edward Kmett
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module is a reimplementation of Edward Kmett's @void@ package.
-- It is included within singletons to avoid depending on @void@ and all the
-- packages that depends on (including @text@). If this causes problems for
-- you (that singletons has its own 'Void' type), please let me (Richard Eisenberg)
-- know at @eir@ at @cis.upenn.edu@.
--
----------------------------------------------------------------------------
module Data.Singletons.Void
  ( Void
  , absurd
  , vacuous
  , vacuousM
  ) where

import Control.Monad (liftM)
import Data.Ix
import Data.Data
import GHC.Generics
import Control.Exception

-- | A logically uninhabited data type.
newtype Void = Void Void
  deriving (Data, Typeable, Generic)

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  showsPrec _ = absurd

-- | Reading a 'Void' value is always a parse error, considering 'Void' as
-- a data type with no constructors.
instance Read Void where
  readsPrec _ _ = []

-- | Since 'Void' values logically don't exist, this witnesses the logical
-- reasoning tool of \"ex falso quodlibet\".
absurd :: Void -> a
absurd a = a `seq` spin a where
   spin (Void b) = spin b

-- | If 'Void' is uninhabited then any 'Functor' that holds only values of type 'Void'
-- is holding no values.
vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd

-- | If 'Void' is uninhabited then any 'Monad' that holds values of type 'Void'
-- is holding no values.
vacuousM :: Monad m => m Void -> m a
vacuousM = liftM absurd

instance Ix Void where
  range _ = []
  index _ = absurd
  inRange _ = absurd
  rangeSize _ = 0

instance Exception Void
