{- Data/Singletons/Promote/Util.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Utility functions used during promotion.
-}

{-# LANGUAGE CPP #-}

module Data.Singletons.Promote.Util where

import Language.Haskell.TH.Syntax
import Data.Singletons.Names
import Language.Haskell.TH.Desugar
import Control.Monad

falseTySym :: DType
falseTySym = promoteValRhs falseName

trueTySym :: DType
trueTySym = promoteValRhs trueName

boolKi :: DKind
boolKi = DConK boolName []

andTySym :: DType
andTySym = promoteValRhs andName

apply :: DType -> DType -> DType
apply t1 t2 = DAppT (DAppT (DConT applyName) t1) t2

-- Get argument kinds from an arrow kind. Removing ForallT is an
-- important preprocessing step required by promoteType.
unravel :: DType -> [DType]
unravel (DForallT _ _ ty) = unravel ty
unravel (DAppT (DAppT DArrowT k1) k2) =
    let ks = unravel k2 in k1 : ks
unravel k = [k]

-- Reconstruct arrow kind from the list of kinds
ravel :: [DType] -> DType
ravel []    = error "Internal error: raveling nil"
ravel [k]   = k
ravel (h:t) = DAppT (DAppT DArrowT h) (ravel t)

-- make a Name with an unknown kind into a DTyVarBndr.
-- Uses a fresh kind variable for GHC 7.6.3 and PlainTV for 7.8+
-- because 7.8+ has kind inference
inferKindTV :: Quasi q => Name -> q (DTyVarBndr, DKind)
inferKindTV n = do
  ki <- fmap DVarK $ qNewName "k"
#if __GLASGOW_HASKELL__ < 707
  return (DKindedTV n ki, ki)
#else
  return (DPlainTV n, ki)
#endif

-- similar to above, this is for annotating the result kind of
-- a closed type family. Makes it polymorphic in 7.6.3, inferred
-- in 7.8
unknownResult :: DKind -> Maybe DKind
#if __GLASGOW_HASKELL__ < 707
unknownResult = Just
#else
unknownResult = const Nothing
#endif
