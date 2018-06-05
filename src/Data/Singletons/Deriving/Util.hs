{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Util
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Utilities used by the `deriving` machinery in singletons.
--
----------------------------------------------------------------------------
module Data.Singletons.Deriving.Util where

import Data.Singletons.Syntax
import Language.Haskell.TH.Desugar

-- A generic type signature for describing how to produce a derived instance.
type DerivDesc q
   = Maybe DCxt  -- (Just ctx) if ctx was provided via StandaloneDeriving.
                 -- Nothing if using a deriving clause.
  -> DType       -- The data type argument to the class.
  -> DataDecl    -- The original data type information.
  -> q UInstDecl -- The derived instance.

-- | Is this data type a non-vanilla data type? Here, \"non-vanilla\" refers to
-- any data type that cannot be expressed using Haskell98 syntax. For instance,
-- this GADT:
--
-- @
-- data Foo :: Type -> Type where
--   MkFoo :: forall a. a -> Foo a
-- @
--
-- Is equivalent to this Haskell98 data type:
--
-- @
-- data Foo a = MkFoo a
-- @
--
-- However, the following GADT is non-vanilla:
--
-- @
-- data Bar :: Type -> Type where
--   MkBar :: Int -> Bar Int
-- @
--
-- Since there is no equivalent Haskell98 data type. The closest you could get
-- is this:
--
-- @
-- data Bar a = (a ~ Int) => MkBar Int
-- @
--
-- Which requires language extensions to write.
--
-- A data type is a non-vanilla if one of the following conditions are met:
--
-- 1. A constructor has any existentially quantified type variables.
--
-- 2. A constructor has a context.
--
-- We care about this because some derivable stock classes, such as 'Enum',
-- forbid derived instances for non-vanilla data types.
isNonVanillaDataType :: forall q. DsMonad q => DType -> [DCon] -> q Bool
isNonVanillaDataType data_ty = anyM $ \con@(DCon _ ctxt _ _ _) -> do
    ex_tvbs <- conExistentialTvbs data_ty con
    return $ not $ null ex_tvbs && null ctxt
  where
    anyM :: (a -> q Bool) -> [a] -> q Bool
    anyM _ [] = return False
    anyM p (x:xs) = do
      b <- p x
      if b then return True else anyM p xs
