-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Partition
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Partitions a list of declarations into its bits
--
----------------------------------------------------------------------------

module Data.Singletons.Partition where

import Prelude hiding ( exp )
import Data.Singletons.Syntax
import Data.Singletons.Deriving.Ord
import Data.Singletons.Deriving.Bounded
import Data.Singletons.Deriving.Enum
import Data.Singletons.Names
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Desugar
import Data.Singletons.Util

import Data.Monoid
import Control.Monad
import Data.Maybe

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

-- | Split up a @[DDec]@ into its pieces, extracting 'Ord' instances
-- from deriving clauses
partitionDecs :: Quasi m => [DDec] -> m PartitionedDecs
partitionDecs = concatMapM partitionDec

partitionDec :: Quasi m => DDec -> m PartitionedDecs
partitionDec (DLetDec letdec) = return $ mempty { pd_let_decs = [letdec] }

partitionDec (DDataD nd _cxt name tvbs cons derivings) = do
  (derivings', derived_instances) <- partitionWithM part_derivings derivings
  return $ mempty { pd_data_decs = [DataDecl nd name tvbs cons derivings']
                  , pd_instance_decs = derived_instances }
  where
    ty = foldType (DConT name) (map tvbToType tvbs)
    part_derivings :: Quasi m => Name -> m (Either Name UInstDecl)
    part_derivings deriv_name
      | deriv_name == ordName
      = Right <$> mkOrdInstance ty cons
      | deriv_name == boundedName
      = Right <$> mkBoundedInstance ty cons
      | deriv_name == enumName
      = Right <$> mkEnumInstance ty cons
      | otherwise
      = return (Left deriv_name)

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
