{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Bool
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines promoted functions and datatypes relating to 'Bool',
-- including a promoted version of all the definitions in @Data.Bool@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Bool@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Bool (
  If,

  -- * Promoted functions from @Data.Bool@
  Bool_, bool_,
  -- | The preceding two definitions are derived from the function 'bool' in
  -- @Data.Bool@. The extra underscore is to avoid name clashes with the type
  -- 'Bool'.

  Not, type (&&), type (||), Otherwise,

  -- * Defunctionalization symbols
  TrueSym0, FalseSym0,

  NotSym0, NotSym1,
  type (&&@#@$), type (&&@#@$$), type (&&@#@$$$),
  type (||@#@$), type (||@#@$$), type (||@#@$$$),
  Bool_Sym0, Bool_Sym1, Bool_Sym2, Bool_Sym3,
  OtherwiseSym0
  ) where

import Data.Singletons.Prelude.Bool
