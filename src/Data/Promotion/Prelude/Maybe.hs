-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Maybe
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines promoted functions and datatypes relating to 'Maybe',
-- including a promoted version of all the definitions in @Data.Maybe@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Maybe@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------


module Data.Promotion.Prelude.Maybe (
  -- * Promoted functions from @Data.Maybe@
  maybe_, Maybe_,
  -- | The preceding two definitions is derived from the function 'maybe' in
  -- @Data.Maybe@. The extra underscore is to avoid name clashes with the type
  -- 'Maybe'.

  IsJust, IsNothing, FromJust, FromMaybe, MaybeToList,
  ListToMaybe, CatMaybes, MapMaybe,

  -- * Defunctionalization symbols
  NothingSym0, JustSym0, JustSym1,

  Maybe_Sym0, Maybe_Sym1, Maybe_Sym2, Maybe_Sym3,
  IsJustSym0, IsJustSym1, IsNothingSym0, IsNothingSym1,
  FromJustSym0, FromJustSym1, FromMaybeSym0, FromMaybeSym1, FromMaybeSym2,
  MaybeToListSym0, MaybeToListSym1, ListToMaybeSym0, ListToMaybeSym1,
  CatMaybesSym0, CatMaybesSym1, MapMaybeSym0, MapMaybeSym1, MapMaybeSym2
  ) where

import Data.Singletons.Prelude.Maybe
