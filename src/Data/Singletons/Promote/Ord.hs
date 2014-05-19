{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Promote.Ord
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of promoted Ord instances
--
----------------------------------------------------------------------------

module Data.Singletons.Promote.Ord where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Util
import Control.Monad

mkOrdTypeInstance :: Quasi q => DKind -> [DCon] -> q [DDec]
mkOrdTypeInstance kind cons = do
  helperName <- newUniqueName "Compare"
  aName <- qNewName "a"
  bName <- qNewName "b"
  let tagged_cons = zip cons [1..]
  (getRankName, getRank) <- buildGetRank tagged_cons
  eqns <- mapM buildEqn cons
  let aVarT = DVarT aName
      bVarT = DVarT bName
      getRankEqn = DTySynEqn [ DSigT aVarT kind
                             , DSigT bVarT kind ]
                         (foldType (DConT tyCompareName)
                             [ DAppT (DConT getRankName) aVarT
                             , DAppT (DConT getRankName) bVarT])
      closedFam = DClosedTypeFamilyD helperName
                                     [ DKindedTV aName kind
                                     , DKindedTV bName kind ]
                                     (Just (DConK orderingName []))
                                     (eqns ++ [getRankEqn])
      ordInst = DTySynInstD tyCompareName
                            (DTySynEqn [ DSigT aVarT kind
                                       , DSigT bVarT kind ]
                                     (foldType (DConT helperName)
                                               [aVarT, bVarT]))
      pord_inst   = DInstanceD [] ((DConT $ promoteClassName ordName) `DAppT`
                                   kindParam kind) [ordInst]

  return [pord_inst,closedFam,getRank]
  where
   buildEqn :: Quasi q => DCon -> q DTySynEqn
   buildEqn (DCon _ _ name fields)
       | null (tysOfConFields fields) =
           return $ DTySynEqn [DConT name, DConT name] (DConT 'EQ)
       | otherwise = do
           let args = length (tysOfConFields fields)
           arg_names1 <- replicateM args (qNewName "x")
           arg_names2 <- replicateM args (qNewName "y")
           let cmps = zipWith (\x y -> foldType (DConT tyCompareName)
                                                [ DVarT x, DVarT y ])
                              arg_names1 arg_names2
               rhs = foldl (\l r -> foldType (DConT tyThenCmpName) [l, r])
                           (DConT 'EQ) cmps
           return $ DTySynEqn [ foldType (DConT name) (map DVarT arg_names1)
                              , foldType (DConT name) (map DVarT arg_names2) ]
                              rhs

buildGetRank :: Quasi q => [(DCon, Int)] -> q (Name, DDec)
buildGetRank cons = do
  getRankName <- newUniqueName "GetRank"
  kvar <- qNewName "a"
  let eqnsData =  map (\(con, tag) ->
                        let (n, ar) = extractNameArgs con in (n, ar, tag))
                      cons
  eqns <- mapM mkGetRankEqn eqnsData
  return $ (getRankName, DClosedTypeFamilyD getRankName
                                            [DPlainTV kvar]
                                            (Just (DConK natName [])) eqns)
      where
        mkGetRankEqn :: Quasi q => (Name, Int, Int) -> q DTySynEqn
        mkGetRankEqn (conName, arity, tag) = do
          args <- replicateM arity (qNewName "a" >>= return . DVarT)
          return $ DTySynEqn [foldType (DConT conName) args]
                             (DLitT (NumTyLit (fromIntegral tag)))
