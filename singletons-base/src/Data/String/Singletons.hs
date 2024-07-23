{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.String.Singletons
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports a promoted and singled version of the 'IsString'
-- type class from "Data.String".
----------------------------------------------------------------------------

module Data.String.Singletons (
  PIsString(..), SIsString(..),

  -- ** Defunctionalization symbols
  FromStringSym0, FromStringSym1
  ) where

import Data.Functor.Const
import Data.Functor.Const.Singletons
import Data.Functor.Identity
import Data.Functor.Identity.Singletons
import Data.Singletons.TH
import GHC.TypeLits (Symbol)
import GHC.TypeLits.Singletons ()   -- for the IsString instance!

$(singletonsOnly [d|
  -- -| Class for string-like datastructures; used by the overloaded string
  --    extension (-XOverloadedStrings in GHC).
  class IsString a where
      fromString :: Symbol -> a

  -- deriving instance IsString a => IsString (Const a (b :: k))
  instance IsString a => IsString (Const a (b :: k)) where
    fromString x = Const (fromString x)

  -- deriving instance IsString a => IsString (Identity a)
  instance IsString a => IsString (Identity a) where
    fromString x = Identity (fromString x)
  |])

-- PIsString instance
instance PIsString Symbol where
  type FromString a = a

-- SIsString instance
instance SIsString Symbol where
  sFromString x = x
