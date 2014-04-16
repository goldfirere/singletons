{- Data/Singletons/LetDecEnv.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Converts a list of DLetDecs into a LetDecEnv for easier processing.
-}

module Data.Singletons.LetDecEnv where

import Prelude hiding ( exp )
import Data.Monoid
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Map ( Map )
import qualified Data.Map as Map

data LetDecRHS = Function [DClause]
               | Value    DExp
data LetDecEnv = LetDecEnv
                 { lde_defns :: Map Name LetDecRHS
                 , lde_types :: Map Name DType   -- type signatures
                 , lde_infix :: [(Fixity, Name)] -- infix declarations
                 }

instance Monoid LetDecEnv where
  mempty = LetDecEnv Map.empty Map.empty []
  mappend (LetDecEnv defns1 types1 infx1) (LetDecEnv defns2 types2 infx2) =
    LetDecEnv (defns1 <> defns2) (types1 <> types2) (infx1 <> infx2)

valueBinding :: Name -> LetDecRHS -> LetDecEnv
valueBinding n v = emptyLetDecEnv { lde_defns = Map.singleton n v }

typeBinding :: Name -> DType -> LetDecEnv
typeBinding n t = emptyLetDecEnv { lde_types = Map.singleton n t }

infixDecl :: Fixity -> Name -> LetDecEnv
infixDecl f n = emptyLetDecEnv { lde_infix = [(f,n)] }

emptyLetDecEnv :: LetDecEnv
emptyLetDecEnv = mempty

buildLetDecEnv :: Quasi q => [DLetDec] -> q LetDecEnv
buildLetDecEnv = go emptyLetDecEnv
  where
    go acc [] = return acc
    go acc (DFunD name clauses : rest) =
      go (valueBinding name (Function clauses) <> acc) rest
    go acc (DValD (DVarPa name) exp : rest) =
      go (valueBinding name (Value exp) <> acc) rest
    go acc (dec@(DValD {}) : rest) = do
      flattened <- flattenDValD dec
      go acc (flattened ++ rest)
    go acc (DSigD name ty : rest) =
      go (typeBinding name ty <> acc) rest
    go acc (DInfixD f n : rest) =
      go (infixDecl f n <> acc) rest
