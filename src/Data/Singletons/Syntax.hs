{- Data/Singletons/Syntax.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Converts a list of DLetDecs into a LetDecEnv for easier processing,
and contains various other AST definitions.
-}

{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, DeriveDataTypeable,
             StandaloneDeriving, FlexibleInstances #-}

module Data.Singletons.Syntax where

import Prelude hiding ( exp )
import Data.Monoid
import Data.Singletons.Util
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Ppr
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad

type VarPromotions = [(Name, Name)]  -- from term-level name to type-level name

  -- the relevant part of declarations
data DataDecl      = DataDecl NewOrData Name [DTyVarBndr] [DCon] [Name]

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

data PartitionedDecs =
  PDecs { pd_let_decs :: [DLetDec]
        , pd_class_decs :: [UClassDecl]
        , pd_instance_decs :: [UInstDecl]
        , pd_data_decs :: [DataDecl]
        }

instance Monoid PartitionedDecs where
  mempty = PDecs [] [] [] []
  mappend (PDecs a1 b1 c1 d1) (PDecs a2 b2 c2 d2) =
    PDecs (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

-- monadic only because of failure
partitionDecs :: Monad m => [DDec] -> m PartitionedDecs
partitionDecs = concatMapM partitionDec

partitionDec :: Monad m => DDec -> m PartitionedDecs
partitionDec (DLetDec letdec) = return $ mempty { pd_let_decs = [letdec] }
partitionDec (DDataD nd _cxt name tvbs cons derivings) =
  return $ mempty { pd_data_decs = [DataDecl nd name tvbs cons derivings] }
partitionDec (DClassD cxt name tvbs fds decs) = do
  env <- concatMapM partitionClassDec decs
  return $ mempty { pd_class_decs = [ClassDecl { cd_cxt  = cxt
                                               , cd_name = name
                                               , cd_tvbs = tvbs
                                               , cd_fds  = fds
                                               , cd_lde  = env }] }
partitionDec (DInstanceD cxt ty decs) = do
  defns <- liftM catMaybes $ mapM partitionInstanceDec decs
  (name, tys) <- split_app_tys [] ty
  return $ mempty { pd_instance_decs = [InstDecl { id_cxt = cxt
                                                 , id_name = name
                                                 , id_arg_tys = tys
                                                 , id_meths = defns }] }
  where
    split_app_tys acc (DAppT t1 t2) = split_app_tys (t2:acc) t1
    split_app_tys acc (DConT name)  = return (name, acc)
    split_app_tys acc (DSigT t _)   = split_app_tys acc t
    split_app_tys _ _ = fail $ "Illegal instance head: " ++ show ty
partitionDec (DRoleAnnotD {}) = return mempty  -- ignore these
partitionDec (DPragmaD {}) = return mempty
partitionDec dec =
  fail $ "Declaration cannot be promoted: " ++ pprint (decToTH dec)

partitionClassDec :: Monad m => DDec -> m ULetDecEnv
partitionClassDec (DLetDec (DSigD name ty)) = return $ typeBinding name ty
partitionClassDec (DLetDec (DValD (DVarPa name) exp)) =
  return $ valueBinding name (UValue exp)
partitionClassDec (DLetDec (DFunD name clauses)) =
  return $ valueBinding name (UFunction clauses)
partitionClassDec (DLetDec (DInfixD fixity name)) =
  return $ infixDecl fixity name
partitionClassDec (DPragmaD {}) = return mempty
partitionClassDec _ =
  fail "Only method declarations can be promoted within a class."

partitionInstanceDec :: Monad m => DDec -> m (Maybe (Name, ULetDecRHS))
partitionInstanceDec (DLetDec (DValD (DVarPa name) exp)) =
  return $ Just (name, UValue exp)
partitionInstanceDec (DLetDec (DFunD name clauses)) =
  return $ Just (name, UFunction clauses)
partitionInstanceDec (DPragmaD {}) = return Nothing
partitionInstanceDec _ =
  fail "Only method bodies can be promoted within an instance."

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
           | ADLamE VarPromotions  -- bind these type variables to these term vars
                    DType          -- the promoted lambda
                    [Name] ADExp
           | ADCaseE ADExp DType [ADMatch] DType
               -- the first type is the promoted scrutinee;
               -- the second type is the return type
           | ADLetE ALetDecEnv ADExp
           | ADSigE ADExp DType

 -- unlike in other places, the DType in an ADMatch (the promoted "case" statement)
 -- should be used with DAppT, *not* apply! (Case statements are not defunctionalized.)
data ADMatch = ADMatch VarPromotions DType DPat ADExp
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
