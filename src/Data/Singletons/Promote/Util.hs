{- Data/Singletons/Promote/Util.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Utility functions used during promotion.
-}

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
