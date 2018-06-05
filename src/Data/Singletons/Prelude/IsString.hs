{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.IsString
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports a promoted and singled version of the 'IsString'
-- type class from "Data.String".
----------------------------------------------------------------------------

module Data.Singletons.Prelude.IsString (
  PIsString(..), SIsString(..),

  -- ** Defunctionalization symbols
  FromStringSym0, FromStringSym1
  ) where

import Data.Singletons.Single
import Data.Singletons.TypeLits ()   -- for the IsString instance!
import GHC.TypeLits (Symbol)

$(singletonsOnly [d|
  -- -| Class for string-like datastructures; used by the overloaded string
  --    extension (-XOverloadedStrings in GHC).
  class IsString a where
      fromString :: Symbol -> a
  |])

-- PIsString instance
instance PIsString Symbol where
  type FromString a = a

-- SIsString instance
instance SIsString Symbol where
  sFromString x = x
