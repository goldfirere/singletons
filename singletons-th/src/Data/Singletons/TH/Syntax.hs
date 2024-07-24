{-# LANGUAGE TypeFamilies #-}

{- Data/Singletons/TH/Syntax.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

Converts a list of DLetDecs into a LetDecEnv for easier processing,
and contains various other AST definitions.
-}

module Data.Singletons.TH.Syntax
  ( module Data.Singletons.TH.Syntax
  , module Data.Singletons.TH.Syntax.LocalVar
  ) where

import Prelude hiding ( exp )
import Data.Kind (Constraint, Type)
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OMap.Strict as OMap
import Language.Haskell.TH.Desugar.OMap.Strict (OMap)
import Language.Haskell.TH.Desugar.OSet (OSet)

import Data.Singletons.TH.Syntax.LocalVar

-- | Pairs of term-level variable 'Name's and their corresponding type-level
-- names (encoded as 'LocalVar's).
type VarPromotions = [(Name, LocalVar)]

-- Information that is accumulated when promoting patterns.
data PromDPatInfos = PromDPatInfos
  { prom_dpat_vars    :: VarPromotions
      -- Maps term-level pattern variables to their promoted, type-level counterparts.
  , prom_dpat_sig_kvs :: OSet LocalVar
      -- Kind variables bound by DSigPas.
      -- See Note [Scoped type variables] in Data.Singletons.TH.Promote.Monad.
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

-- The parts of data declarations that are relevant to singletons-th.
data DataDecl = DataDecl DataFlavor Name [DTyVarBndrVis] [DCon]

-- The parts of type synonyms that are relevant to singletons-th.
data TySynDecl = TySynDecl Name [DTyVarBndrVis] DType

-- The parts of open type families that are relevant to singletons-th.
type OpenTypeFamilyDecl = TypeFamilyDecl 'Open

-- The parts of closed type families that are relevant to singletons-th.
type ClosedTypeFamilyDecl = TypeFamilyDecl 'Closed

-- The parts of type families that are relevant to singletons-th.
newtype TypeFamilyDecl (info :: FamilyInfo)
  = TypeFamilyDecl { getTypeFamilyDecl :: DTypeFamilyHead }
-- Whether a type family is open or closed.
data FamilyInfo = Open | Closed

data ClassDecl ann
  = ClassDecl { cd_cxt  :: DCxt
              , cd_name :: Name
              , cd_tvbs :: [DTyVarBndrVis]
              , cd_fds  :: [FunDep]
              , cd_lde  :: LetDecEnv ann
              , cd_atfs :: [OpenTypeFamilyDecl]
                  -- Associated type families. Only recorded for
                  -- defunctionalization purposes.
                  -- See Note [Partitioning, type synonyms, and type families]
                  -- in D.S.TH.Partition.
              }

data InstDecl  ann = InstDecl { id_cxt     :: DCxt
                              , id_name    :: Name
                              , id_arg_tys :: [DType]
                              , id_sigs    :: OMap Name DType
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
           | ADLamCasesE
               Int
               -- ^ The number of arguments in each clause. Although this can be
               -- computed from the list of ADClauses, this information is used
               -- multiple times during promotion and singling, so we cache this
               -- number here as a convenience.
               DType
               -- ^ The promoted lambda.
               [ADClause]
               -- ^ The list of clauses in the @\\cases@ expression.
           | ADLetE ALetDecEnv ADExp
           | ADSigE DType          -- the promoted expression
                    ADExp DType

-- A DPat with a pattern-signature node annotated with its type-level equivalent
data ADPat = ADLitP Lit
           | ADVarP Name
           | ADConP Name [DType] [ADPat]
           | ADTildeP ADPat
           | ADBangP ADPat
           | ADSigP DType -- The promoted pattern. Will not contain any wildcards,
                          -- as per Note [Singling pattern signatures]
                    ADPat DType
           | ADWildP

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
  = -- A function definition. Invariant: each ADClause contains at least one
    -- pattern.
    AFunction
      Int -- The number of arrows in the type. As a consequence of the invariant
          -- above, this is always a positive number.
      [ADClause]

  | -- A value whose definition is given by the DExp. Invariant: the value is
    -- not a function (i.e., there are no occurrences of (->) in the value's
    -- type).
    AValue
      ADExp
data instance LetDecRHS Unannotated = UFunction [DClause]
                                    | UValue DExp

type ALetDecRHS = LetDecRHS Annotated
type ULetDecRHS = LetDecRHS Unannotated

-- | A @let@-bound, term-level name that is promoted to the type level. The
-- first element of the pair (of type 'Name') is the promoted counterpart to the
-- term-level name, and the second element of the pair (of type @[Name]@) is the
-- list of local variables that this definition closes over after being
-- lambda-lifted. (See @Note [Tracking local variables]@ in
-- "Data.Singletons.TH.Promote.Monad".)
--
-- Note that the promoted Name in the first element of the pair is /not/ a
-- defunctionalization symbol, unlike 'LetBind' in
-- "Data.Singletons.TH.Promote.Monad". This is because it is sometimes
-- convenient to fully apply the promoted name to all of its arguments (e.g.,
-- when singling type signatures), in which case we can avoid needing to involve
-- defunctionalization symbols at all.
type LetDecProm = (Name, [LocalVar])

data LetDecEnv ann = LetDecEnv
                   { lde_defns :: OMap Name (LetDecRHS ann)
                   , lde_types :: OMap Name DType  -- type signatures
                   , lde_infix :: OMap Name (Fixity, NamespaceSpecifier) -- infix declarations
                   , lde_proms :: IfAnn ann (OMap Name LetDecProm) ()
                     -- ^ If annotated, this maps let-bound term 'Name's to
                     -- their promoted counterparts.
                   }
type ALetDecEnv = LetDecEnv Annotated
type ULetDecEnv = LetDecEnv Unannotated

instance Semigroup ULetDecEnv where
  LetDecEnv defns1 types1 infx1 _ <> LetDecEnv defns2 types2 infx2 _ =
    LetDecEnv (defns1 <> defns2) (types1 <> types2) (infx1 <> infx2) ()

instance Monoid ULetDecEnv where
  mempty = LetDecEnv OMap.empty OMap.empty OMap.empty ()

valueBinding :: Name -> ULetDecRHS -> ULetDecEnv
valueBinding n v = emptyLetDecEnv { lde_defns = OMap.singleton n v }

typeBinding :: Name -> DType -> ULetDecEnv
typeBinding n t = emptyLetDecEnv { lde_types = OMap.singleton n t }

infixDecl :: Fixity -> NamespaceSpecifier -> Name -> ULetDecEnv
infixDecl f ns n = emptyLetDecEnv { lde_infix = OMap.singleton n (f, ns) }

emptyLetDecEnv :: ULetDecEnv
emptyLetDecEnv = mempty

buildLetDecEnv :: Quasi q => [DLetDec] -> q ULetDecEnv
buildLetDecEnv = go emptyLetDecEnv
  where
    go acc [] = return acc
    go acc (DFunD name clauses : rest) =
      go (valueBinding name (UFunction clauses) <> acc) rest
    go acc (DValD (DVarP name) exp : rest) =
      go (valueBinding name (UValue exp) <> acc) rest
    go acc (dec@(DValD {}) : rest) = do
      flattened <- flattenDValD dec
      go acc (flattened ++ rest)
    go acc (DSigD name ty : rest) =
      go (typeBinding name ty <> acc) rest
    go acc (DInfixD f ns n : rest) =
      go (infixDecl f ns n <> acc) rest
    go acc (DPragmaD{} : rest) = go acc rest

-- See Note [DerivedDecl]
data DerivedDecl (cls :: Type -> Constraint) = DerivedDecl
  { ded_mb_cxt     :: Maybe DCxt
  , ded_type       :: DType
  , ded_type_tycon :: Name
  , ded_decl       :: DataDecl
  }

type DerivedEqDecl   = DerivedDecl Eq
type DerivedOrdDecl  = DerivedDecl Ord
type DerivedShowDecl = DerivedDecl Show

{- Note [DerivedDecl]
~~~~~~~~~~~~~~~~~~~~~
Most derived instances are wholly handled in
Data.Singletons.TH.Partition.partitionDecs. There are two notable exceptions to
this rule, however, that are partially handled outside of partitionDecs:
Eq and Show instances. For these instances, we use a DerivedDecl data type to
encode just enough information to recreate the derived instance:

1. Just the instance context, if it's standalone-derived, or Nothing if it's in
   a deriving clause (ded_mb_cxt)
2. The datatype, applied to some number of type arguments, as in the
   instance declaration (ded_type)
3. The datatype name (ded_type_tycon), cached for convenience
4. The datatype's constructors (ded_cons)

Why are these instances handled outside of partitionDecs?

* Deriving Eq in singletons-th not only derives PEq/SEq instances, but it also
  derives SDecide, Eq, TestEquality, and TestCoercion instances.
* Deriving Ord in singletons-th not only derives POrd/SOrd instances, but it also
  derives Ord instances for the singleton types themselves.
* Deriving Show in singletons-th not only derives PShow/SShow instances, but it
  also derives Show instances for the singleton types themselves.

To make this work, we let partitionDecs handle the P{Eq,Show} and S{Eq,Show}
instances, but we also stick the relevant info into a DerivedDecl value for
later use in Data.Singletons.TH.Single, where we additionally generate
SDecide, Eq, TestEquality, TestCoercion and Show instances for singleton types.
-}
