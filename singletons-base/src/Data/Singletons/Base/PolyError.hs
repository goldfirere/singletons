{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Base.TypeError
-- Copyright   :  (C) 2023 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines a replacement for the promoted @Error@ function whose argument is
-- kind-polymorphic.
--
----------------------------------------------------------------------------
module Data.Singletons.Base.PolyError (PolyError) where

import Data.Singletons.TH

-- | Like @Error@ from "GHC.TypeLits.Singletons", but with an argument that is
-- generalized to be kind-polymorphic. This allows passing additional
-- information to the error besides raw @Symbol@s.
type PolyError :: a -> b
type family PolyError (arg :: a) :: b where {}
$(genDefunSymbols [''PolyError])
