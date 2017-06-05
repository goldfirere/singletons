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
        , pd_derived_eq_decs :: [DerivedEqDecl]
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
  derived_decs
    <- mapM (\(strat, deriv_pred) -> partitionDeriving strat deriv_pred Nothing ty cons)
      $ concatMap flatten_clause derivings
  return $ mconcat $ data_dec : derived_decs
  where
    data_dec = mempty { pd_data_decs = [DataDecl nd name tvbs cons []] }
    ty = foldType (DConT name) (map tvbToType tvbs)

    flatten_clause :: DDerivClause -> [(Maybe DerivStrategy, DPred)]
    flatten_clause (DDerivClause strat preds) = map (strat,) preds

    part_derivings :: DsMonad m => (Maybe DerivStrategy, DPred)
                              -> m (Either DPred UInstDecl)
    part_derivings (strat, deriv) = case deriv of
      DConPr deriv_name
         | stock, deriv_name == ordName
        -> Right <$> mkOrdInstance ty cons
         | stock, deriv_name == boundedName
        -> Right <$> mkBoundedInstance ty cons
         | stock, deriv_name == enumName
        -> Right <$> mkEnumInstance ty cons
         | stock, deriv_name == showName
        -> Right <$> mkShowInstance ty cons
        where
          stock = case strat of
                    Nothing            -> True
                    Just StockStrategy -> True
                    Just _             -> False
      _ -> return (Left deriv)

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
      | not (null cls_tys) -- We can't handle zero-parameter type classes
      , let cls_arg_tys  = init cls_tys
            data_ty      = last cls_tys
            data_ty_head = case unfoldType data_ty of ty_head :| _ -> ty_head
      , DConT data_tycon <- data_ty_head -- We can't handle deriving an instance for something
                                         -- other than a type constructor application
      -> do let cls_pred = foldType cls_pred_ty cls_arg_tys
            dinfo <- dsReify data_tycon
            case dinfo of
              Just (DTyConI (DDataD _ _ _ _ cons _) _) -> do
                partitionDeriving mb_strat cls_pred (Just ctxt) data_ty cons
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

partitionDeriving :: DsMonad m => Maybe DerivStrategy -> DType -> Maybe DCxt -> DType -> [DCon]
                  -> m PartitionedDecs
partitionDeriving mb_strat deriv_pred mb_ctxt ty cons =
  case unfoldType deriv_pred of
    DConT deriv_name :| arg_tys
         -- Here, we are more conservative than GHC: DeriveAnyClass only kicks
         -- in if the user explicitly chooses to do so with the anyclass
         -- deriving strategy
       | Just AnyclassStrategy <- mb_strat
      -> return $ mk_derived_inst
           InstDecl { id_cxt = fromMaybe [] mb_ctxt
                      -- For now at least, there's no point in attempting to
                      -- infer an instance context for DeriveAnyClass, since
                      -- the other language feature that requires it,
                      -- DefaultSignatures, can't be singled. Thus, inferring an
                      -- empty context will Just Work for all currently supported
                      -- default implementations.
                      --
                      -- (Of course, if a user specifies a context with
                      -- StandaloneDeriving, use that.)

                    , id_name    = deriv_name
                    , id_arg_tys = arg_tys ++ [ty]
                    , id_meths   = [] }

       | Just NewtypeStrategy <- mb_strat
      -> do qReportWarning "GeneralizedNewtypeDeriving is ignored by `singletons`."
            return mempty

    -- Stock classes. These are derived only if `singletons` supports them
    -- (and, optionally, if an explicit stock deriving strategy is used)
    DConT deriv_name :| [] -- For now, all stock derivable class supported in
                           -- singletons take just one argument (the data
                           -- type itself)
       | stock_or_default
       , deriv_name == ordName
      -> mk_derived_inst <$> mkOrdInstance mb_ctxt ty cons

       | stock_or_default
       , deriv_name == boundedName
      -> mk_derived_inst <$> mkBoundedInstance mb_ctxt ty cons

       | stock_or_default
       , deriv_name == enumName
      -> mk_derived_inst <$> mkEnumInstance mb_ctxt ty cons

       | stock_or_default
       , deriv_name == showName
      -> mk_derived_inst <$> mkShowInstance mb_ctxt ty cons

         -- See Note [Derived Eq instances] in Data.Singletons.Syntax
       | stock_or_default
       , deriv_name == eqName
      -> return $ mk_derived_eq_inst
           DerivedEqDecl { ded_mb_cxt = mb_ctxt
                         , ded_type   = ty
                         , ded_cons   = cons }

         -- If we can't find a stock class, but the user bothered to use an
         -- explicit stock keyword, we can at least warn them about it.
       | Just StockStrategy <- mb_strat
      -> do qReportWarning $ "`singletons` doesn't recognize the stock class "
                             ++ nameBase deriv_name
            return mempty

    _ -> return mempty -- singletons doesn't support deriving this instance
  where
      mk_derived_inst    dec = mempty { pd_instance_decs   = [dec] }
      mk_derived_eq_inst dec = mempty { pd_derived_eq_decs = [dec] }
      stock_or_default = isStockOrDefault mb_strat

-- Is this being used with an explicit stock strategy, or no strategy at all?
isStockOrDefault :: Maybe DerivStrategy -> Bool
isStockOrDefault Nothing              = True
isStockOrDefault (Just StockStrategy) = True
isStockOrDefault (Just _)             = False
