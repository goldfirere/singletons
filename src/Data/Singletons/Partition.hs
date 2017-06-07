-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Partition
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Partitions a list of declarations into its bits
--
----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Data.Singletons.Partition where

import Prelude hiding ( exp )
import Data.Singletons.Syntax
import Data.Singletons.Deriving.Ord
import Data.Singletons.Deriving.Bounded
import Data.Singletons.Deriving.Enum
import Data.Singletons.Deriving.Show
import Data.Singletons.Names
import Language.Haskell.TH.Syntax hiding (showName)
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Desugar
import Data.Singletons.Util

import Data.Monoid
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe

data PartitionedDecs =
  PDecs { pd_let_decs :: [DLetDec]
        , pd_class_decs :: [UClassDecl]
        , pd_instance_decs :: [UInstDecl]
        , pd_data_decs :: [DataDecl]
        , pd_standalone_derived_eq_decs :: [StandaloneDerivedEqDec]
        }

instance Monoid PartitionedDecs where
  mempty = PDecs [] [] [] [] []
  mappend (PDecs a1 b1 c1 d1 e1) (PDecs a2 b2 c2 d2 e2) =
    PDecs (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

-- | Split up a @[DDec]@ into its pieces, extracting 'Ord' instances
-- from deriving clauses
partitionDecs :: DsMonad m => [DDec] -> m PartitionedDecs
partitionDecs = concatMapM partitionDec

partitionDec :: DsMonad m => DDec -> m PartitionedDecs
partitionDec (DLetDec (DPragmaD {})) = return mempty
partitionDec (DLetDec letdec) = return $ mempty { pd_let_decs = [letdec] }

partitionDec (DDataD nd _cxt name tvbs cons derivings) = do
  (derivings', derived_instances)
    <- partitionWithM (\(strat, deriv_pred) -> partitionDeriving strat deriv_pred Nothing ty cons)
      $ concatMap flatten_clause derivings
  return $ mempty { pd_data_decs = [DataDecl nd name tvbs cons derivings']
                  , pd_instance_decs = derived_instances }
  where
    ty = foldType (DConT name) (map tvbToType tvbs)

    flatten_clause :: DDerivClause -> [(Maybe DerivStrategy, DPred)]
    flatten_clause (DDerivClause strat preds) = map (strat,) preds

partitionDec (DClassD cxt name tvbs fds decs) = do
  env <- concatMapM partitionClassDec decs
  return $ mempty { pd_class_decs = [ClassDecl { cd_cxt  = cxt
                                               , cd_name = name
                                               , cd_tvbs = tvbs
                                               , cd_fds  = fds
                                               , cd_lde  = env }] }
partitionDec (DInstanceD _ cxt ty decs) = do
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
partitionDec (DTySynD {})     = return mempty  -- ignore type synonyms;
                                               -- promotion is a no-op, and
                                               -- singling expands all syns
partitionDec (DStandaloneDerivD mb_strat ctxt ty) =
  case unfoldType ty of
    cls_pred_ty :| cls_tys
      | DConT cls_pred_tycon <- cls_pred_ty
      , not (null cls_tys) -- We can't handle zero-parameter type classes
      , let data_ty = last cls_tys
            data_ty_head = case unfoldType data_ty of ty_head :| _ -> ty_head
      , DConT data_tycon <- data_ty_head -- We can't handle deriving an instance for something
                                         -- other than a type constructor application
      -> do let cls_pred = DConPr cls_pred_tycon
            dinfo <- dsReify data_tycon
            case dinfo of
              Just (DTyConI (DDataD _ _ _ _ cons _) _) -> do
                mb_instance <- partitionDeriving mb_strat cls_pred (Just ctxt) data_ty cons
                case mb_instance of
                  Left _ -> case cls_pred of
                              -- See Note [Standalone derived Eq instances]
                              DConPr cls_name
                                | isStock mb_strat, cls_name == eqName
                                -> let sded = SDEqDec { sded_cxt  = ctxt
                                                      , sded_type = data_ty
                                                      , sded_cons = cons }
                                   in return mempty { pd_standalone_derived_eq_decs = [sded] }
                              _ -> return mempty -- singletons doesn't support deriving this instance
                  Right derived_instance -> return $ mempty { pd_instance_decs = [derived_instance] }
              Just _ ->
                fail $ "Standalone derived instance for something other than a datatype: "
                       ++ show data_ty
              _ -> fail $ "Cannot find " ++ show data_ty
    _ -> return mempty
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
partitionClassDec (DLetDec (DPragmaD {})) = return mempty
partitionClassDec _ =
  fail "Only method declarations can be promoted within a class."

partitionInstanceDec :: Monad m => DDec -> m (Maybe (Name, ULetDecRHS))
partitionInstanceDec (DLetDec (DValD (DVarPa name) exp)) =
  return $ Just (name, UValue exp)
partitionInstanceDec (DLetDec (DFunD name clauses)) =
  return $ Just (name, UFunction clauses)
partitionInstanceDec (DLetDec (DPragmaD {})) = return Nothing
partitionInstanceDec _ =
  fail "Only method bodies can be promoted within an instance."

partitionDeriving :: DsMonad m => Maybe DerivStrategy -> DPred -> Maybe DCxt -> DType -> [DCon]
                  -> m (Either DPred UInstDecl)
partitionDeriving mb_strat deriv_pred mb_ctxt ty cons = case deriv_pred of
  DConPr deriv_name
     | stock, deriv_name == ordName
    -> Right <$> mkOrdInstance mb_ctxt ty cons
     | stock, deriv_name == boundedName
    -> Right <$> mkBoundedInstance mb_ctxt ty cons
     | stock, deriv_name == enumName
    -> Right <$> mkEnumInstance mb_ctxt ty cons
     | stock, deriv_name == showName
    -> Right <$> mkShowInstance mb_ctxt ty cons
    where
      stock = isStock mb_strat
  _ -> return (Left deriv_pred)

isStock :: Maybe DerivStrategy -> Bool
isStock Nothing = True -- We assume the lack of an explicit deriving strategy to
                       -- indicate defaulting to stock. In reality, GHC's defaulting
                       -- behavior is much more complex than this, but implementing
                       -- this in singletons would be impossible, as would require
                       -- detecting the presence of extensions.
isStock (Just StockStrategy) = True
isStock (Just _)             = False
