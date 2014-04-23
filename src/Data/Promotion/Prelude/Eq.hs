-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Eq
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Re-export promoted definitions related to type-level equality.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExplicitNamespaces #-}
module Data.Promotion.Prelude.Eq (
  type (==), (:==), (:/=), (:==$), (:==$$), (:==$$$), (:/=$), (:/=$$), (:/=$$$)
  ) where

import Data.Singletons.Prelude.Eq
