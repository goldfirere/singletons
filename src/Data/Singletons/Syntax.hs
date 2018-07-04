{- Data/Singletons/Syntax.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

Converts a list of DLetDecs into a LetDecEnv for easier processing,
and contains various other AST definitions.
-}

{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, DeriveDataTypeable,
             StandaloneDeriving, FlexibleInstances, ConstraintKinds #-}

module Data.Singletons.Syntax where

import Prelude hiding ( exp )
import Data.Kind (Constraint, Type)
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.TH.Desugar
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import Data.Semigroup (Semigroup(..))

type VarPromotions = [(Name, Name)] -- from term-level name to type-level name

-- Information that is accumulated when promoting patterns.
data PromDPatInfos = PromDPatInfos
  { prom_dpat_vars    :: VarPromotions
      -- Maps term-level pattern variables to their promoted, type-level counterparts.
  , prom_dpat_sig_kvs :: Set Name
      -- Kind variables bound by DSigPas.
      -- See Note [Explicitly binding kind variables] in Data.Singletons.Promote.Monad
  }

instance Semigroup PromDPatInfos where
  PromDPatInfos vars1 sig_kvs1 <> PromDPatInfos vars2 sig_kvs2
    = PromDPatInfos (vars1 <> vars2) (sig_kvs1 <> sig_kvs2)

instance Monoid PromDPatInfos where
  mempty = PromDPatInfos mempty mempty

-- A list of 'SingDSigPaInfos' is produced when singling pattern signatures, as we
-- must case on the 'DExp's and match on them using the supplied 'DType's to
-- bring the necessary singleton equality constraints into scope.
-- See @Note [Singling pattern signatures]@.
type SingDSigPaInfos = [(DExp, DType)]

-- The parts of data declarations that are relevant to singletons.
data DataDecl = DataDecl Name [DTyVarBndr] [DCon]

-- The parts of type synonyms that are relevant to singletons.
data TySynDecl = TySynDecl Name [DTyVarBndr]

-- The parts of open type families that are relevant to singletons.
type OpenTypeFamilyDecl = TypeFamilyDecl 'Open

-- The parts of closed type families that are relevant to singletons.
type ClosedTypeFamilyDecl = TypeFamilyDecl 'Closed

-- The parts of type families that are relevant to singletons.
newtype TypeFamilyDecl (info :: FamilyInfo)
  = TypeFamilyDecl { getTypeFamilyDecl :: DTypeFamilyHead }
-- Whether a type family is open or closed.
data FamilyInfo = Open | Closed

data ClassDecl ann = ClassDecl { cd_cxt  :: DCxt
                               , cd_name :: Name
                               , cd_tvbs :: [DTyVarBndr]
                               , cd_fds  :: [FunDep]
                               , cd_lde  :: LetDecEnv ann
                               }

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

-- A DExp with let, lambda, and type-signature nodes annotated with their
-- type-level equivalents
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
           | ADSigE DType          -- the promoted expression
                    ADExp DType

-- A DPat with a pattern-signature node annotated with its type-level equivalent
data ADPat = ADLitPa Lit
           | ADVarPa Name
           | ADConPa Name [ADPat]
           | ADTildePa ADPat
           | ADBangPa ADPat
           | ADSigPa DType -- The promoted pattern. Will not contain any wildcards,
                           -- as per Note [Singling pattern signatures]
                     ADPat DType
           | ADWildPa

data ADMatch = ADMatch VarPromotions ADPat ADExp
data ADClause = ADClause VarPromotions
                         [ADPat] ADExp

data AnnotationFlag = Annotated | Unannotated

-- These are used at the type-level exclusively
type Annotated   = 'Annotated
type Unannotated = 'Unannotated

type family IfAnn (ann :: AnnotationFlag) (yes :: k) (no :: k) :: k where
  IfAnn Annotated   yes no = yes
  IfAnn Unannotated yes no = no

data family LetDecRHS :: AnnotationFlag -> Type
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
                   , lde_infix :: Map Name Fixity  -- infix declarations
                   , lde_proms :: IfAnn ann (Map Name DType) () -- possibly, promotions
                   , lde_bound_kvs :: IfAnn ann (Map Name (Set Name)) ()
                     -- The set of bound variables in scope.
                     -- See Note [Explicitly binding kind variables]
                     -- in Data.Singletons.Promote.Monad
                   }
type ALetDecEnv = LetDecEnv Annotated
type ULetDecEnv = LetDecEnv Unannotated

instance Semigroup ULetDecEnv where
  LetDecEnv defns1 types1 infx1 _ _ <> LetDecEnv defns2 types2 infx2 _ _ =
    LetDecEnv (defns1 <> defns2) (types1 <> types2) (infx1 <> infx2) () ()

instance Monoid ULetDecEnv where
  mempty = LetDecEnv Map.empty Map.empty Map.empty () ()

valueBinding :: Name -> ULetDecRHS -> ULetDecEnv
valueBinding n v = emptyLetDecEnv { lde_defns = Map.singleton n v }

typeBinding :: Name -> DType -> ULetDecEnv
typeBinding n t = emptyLetDecEnv { lde_types = Map.singleton n t }

infixDecl :: Fixity -> Name -> ULetDecEnv
infixDecl f n = emptyLetDecEnv { lde_infix = Map.singleton n f }

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

-- See Note [DerivedDecl]
data DerivedDecl (cls :: Type -> Constraint) = DerivedDecl
  { ded_mb_cxt :: Maybe DCxt
  , ded_type   :: DType
  , ded_decl   :: DataDecl
  }

type DerivedEqDecl   = DerivedDecl Eq
type DerivedShowDecl = DerivedDecl Show

{- Note [DerivedDecl]
~~~~~~~~~~~~~~~~~~~~~
Most derived instances are wholly handled in
Data.Singletons.Partition.partitionDecs. There are two notable exceptions to
this rule, however:

* Eq instances (which are handled entirely outside of partitionDecs)
* Show instances (which are partially handled outside of partitionDecs)

For these instances, we use a DerivedDecl data type to encode just enough
information to recreate the derived instance:

1. Just the instance context, if it's standalone-derived, or Nothing if it's in
   a deriving clause (ded_mb_cxt)
2. The datatype, applied to some number of type arguments, as in the
   instance declaration (ded_type)
3. The datatype's original information, as provided through DataDecl (ded_decl)

Why are these instances handled outside of partitionDecs?

* Deriving Eq in singletons not only derives PEq/SEq instances, but it also
  derives SDecide instances. This additional complication makes Eq difficult
  to integrate with the other deriving machinery, so we handle it specially
  in Data.Singletons.Promote and Data.Singletons.Single (depending on the task
  at hand).
* Deriving Show in singletons not only derives PShow/SShow instances, but it
  also derives Show instances for singletons types. To make this work,
  we let partitionDecs handle the PShow/SShow instances, but we also stick the
  relevant info into a DerivedDecl value for later use in
  Data.Singletons.Single, where we additionally generate Show
  instances.
-}
