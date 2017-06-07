{- Data/Singletons/Syntax.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

Converts a list of DLetDecs into a LetDecEnv for easier processing,
and contains various other AST definitions.
-}

{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, DeriveDataTypeable,
             StandaloneDeriving, FlexibleInstances #-}

module Data.Singletons.Syntax where

import Prelude hiding ( exp )
import Data.Monoid
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

type VarPromotions = [(Name, Name)]  -- from term-level name to type-level name

  -- the relevant part of declarations
data DataDecl      = DataDecl NewOrData Name [DTyVarBndr] [DCon] [DPred]

data ClassDecl ann = ClassDecl { cd_cxt  :: DCxt
                               , cd_name :: Name
                               , cd_tvbs :: [DTyVarBndr]
                               , cd_fds  :: [FunDep]
                               , cd_lde  :: LetDecEnv ann }

data InstDecl  ann = InstDecl { id_cxt     :: DCxt
                              , id_name    :: Name
                              , id_arg_tys :: [DType]
                              , id_meths   :: [(Name, LetDecRHS ann)] }

type UClassDecl = ClassDecl Unannotated
type UInstDecl  = InstDecl Unannotated

type AClassDecl = ClassDecl Annotated
type AInstDecl  = InstDecl Annotated

{-
We see below several datatypes beginning with "A". These are annotated structures,
necessary for Promote to communicate key things to Single. In particular, promotion
of expressions is *not* deterministic, due to the necessity to create unique names
for lets, cases, and lambdas. So, we put these promotions into an annotated AST
so that Single can use the right promotions.
-}

-- A DExp with let and lambda nodes annotated with their type-level equivalents
data ADExp = ADVarE Name
           | ADConE Name
           | ADLitE Lit
           | ADAppE ADExp ADExp
           | ADLamE [Name]         -- type-level names corresponding to term-level ones
                    DType          -- the promoted lambda
                    [Name] ADExp
           | ADCaseE ADExp [ADMatch] DType
               -- the type is the return type
           | ADLetE ALetDecEnv ADExp
           | ADSigE ADExp DType

data ADMatch = ADMatch VarPromotions DPat ADExp
data ADClause = ADClause VarPromotions
                         [DPat] ADExp

data AnnotationFlag = Annotated | Unannotated

-- These are used at the type-level exclusively
type Annotated   = 'Annotated
type Unannotated = 'Unannotated

type family IfAnn (ann :: AnnotationFlag) (yes :: k) (no :: k) :: k
type instance IfAnn Annotated   yes no = yes
type instance IfAnn Unannotated yes no = no

data family LetDecRHS (ann :: AnnotationFlag)
data instance LetDecRHS Annotated
  = AFunction DType  -- promote function (unapplied)
    Int    -- number of arrows in type
    [ADClause]
  | AValue DType -- promoted exp
    Int   -- number of arrows in type
    ADExp
data instance LetDecRHS Unannotated = UFunction [DClause]
                                    | UValue DExp

type ALetDecRHS = LetDecRHS Annotated
type ULetDecRHS = LetDecRHS Unannotated

data LetDecEnv ann = LetDecEnv
                   { lde_defns :: Map Name (LetDecRHS ann)
                   , lde_types :: Map Name DType   -- type signatures
                   , lde_infix :: [(Fixity, Name)] -- infix declarations
                   , lde_proms :: IfAnn ann (Map Name DType) () -- possibly, promotions
                   }
type ALetDecEnv = LetDecEnv Annotated
type ULetDecEnv = LetDecEnv Unannotated

instance Monoid ULetDecEnv where
  mempty = LetDecEnv Map.empty Map.empty [] ()
  mappend (LetDecEnv defns1 types1 infx1 _) (LetDecEnv defns2 types2 infx2 _) =
    LetDecEnv (defns1 <> defns2) (types1 <> types2) (infx1 <> infx2) ()

valueBinding :: Name -> ULetDecRHS -> ULetDecEnv
valueBinding n v = emptyLetDecEnv { lde_defns = Map.singleton n v }

typeBinding :: Name -> DType -> ULetDecEnv
typeBinding n t = emptyLetDecEnv { lde_types = Map.singleton n t }

infixDecl :: Fixity -> Name -> ULetDecEnv
infixDecl f n = emptyLetDecEnv { lde_infix = [(f,n)] }

emptyLetDecEnv :: ULetDecEnv
emptyLetDecEnv = mempty

buildLetDecEnv :: Quasi q => [DLetDec] -> q ULetDecEnv
buildLetDecEnv = go emptyLetDecEnv
  where
    go acc [] = return acc
    go acc (DFunD name clauses : rest) =
      go (valueBinding name (UFunction clauses) <> acc) rest
    go acc (DValD (DVarPa name) exp : rest) =
      go (valueBinding name (UValue exp) <> acc) rest
    go acc (dec@(DValD {}) : rest) = do
      flattened <- flattenDValD dec
      go acc (flattened ++ rest)
    go acc (DSigD name ty : rest) =
      go (typeBinding name ty <> acc) rest
    go acc (DInfixD f n : rest) =
      go (infixDecl f n <> acc) rest
    go acc (DPragmaD{} : rest) = go acc rest

-- See Note [Standalone derived Eq instances]
data StandaloneDerivedEqDec = SDEqDec
  { sded_cxt  :: DCxt
  , sded_type :: DType
  , sded_cons :: [DCon]
  }

{- Note [Standalone derived Eq instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most standalone derived instances are handled in
Data.Singletons.Partition.partitionDecs. There is one notable exception to this
rule, however: Eq instances. The reason is because deriving Eq in singletons
not only derives PEq/SEq instances, but it also derives SDecide instances.
This additional complication makes Eq difficult to integrate with the other
deriving machinery, so we handle it specially in Data.Singletons.Promote
and Data.Singletons.Single (depending on the task at hand).

The StandaloneDerivedEqDec type encodes just enough information to recreate
the standalone derived Eq instance:

1. The instance context (sded_cxt)
2. The datatype, applied to some number of type arguments, as in the
   instance declaration (sded_type)
3. The datatype's constructors (sded_cons)
-}
