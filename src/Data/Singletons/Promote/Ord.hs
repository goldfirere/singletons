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
import Data.List

mkOrdTypeInstance :: Quasi q => DKind -> [DCon] -> q [DDec]
mkOrdTypeInstance kind cons = do
  let taggedCons   = zip cons [1..]
      l            = length cons
      half         = l `div` 2 + l `mod` 2
      combinations = [ (x,y) | x@(_, t1) <- taggedCons
                             , y@(_, t2) <- taggedCons
                             , (t1 <= half && t2 <= t1 + 1) ||
                               (t1 >  half && t2 >= t1 - 1) ]
      groupedCombs = groupBy equalFirstTags combinations
      equalFirstTags ((_,t1),_) ((_,t2),_) = t1 == t2
      reverseOrder [] = []
      reverseOrder xs@(((_,t),_):_) = if t > half
                                      then reverse xs
                                      else xs
      consPairs    = concat (map reverseOrder groupedCombs)
  helperName <- newUniqueName "Compare"
  aName <- qNewName "a"
  bName <- qNewName "b"
  compareEqns <- mapM (mkCompareEqn half) consPairs
  let closedFam = DClosedTypeFamilyD helperName
                                     [ DKindedTV aName kind
                                     , DKindedTV bName kind ]
                                     (Just (DConK orderingName []))
                                     compareEqns
      compareInst = DTySynInstD tyCompareName
                               (DTySynEqn [ DSigT (DVarT aName) kind
                                          , DSigT (DVarT bName) kind ]
                                          (foldType (DConT helperName)
                                                    [DVarT aName, DVarT bName]))
  return [closedFam, compareInst]

  where mkCompareEqn :: Quasi q => Int -> ((DCon, Int), (DCon, Int)) -> q DTySynEqn
        mkCompareEqn half ((con1, tag1), (con2, tag2))
            | tag1 > tag2 && tag1 <= half =
                mkCompareEqnHelper con1 (Just con2) ordGTSymName
            | tag1 < tag2 && tag1 >  half = do
                mkCompareEqnHelper con1 (Just con2) ordLTSymName
            | tag1 < tag2 && tag1 <= half =
                mkCompareEqnHelper con1 Nothing ordLTSymName
            | tag1 > tag2 && tag1 >  half =
                mkCompareEqnHelper con1 Nothing ordGTSymName
            | otherwise = -- WRONG!
                mkCompareEqnHelper con1 (Just con2) ordEQSymName

        mkCompareEqnHelper :: Quasi q => DCon -> Maybe DCon -> Name -> q DTySynEqn
        mkCompareEqnHelper con1 con2 result = do
            let (name1, numArgs1) = extractNameArgs con1
            (name2, numArgs2) <- case con2 of
                  Just c  -> let (n, numArgs) = extractNameArgs c
                             in  return (DConT n, numArgs)
                  Nothing -> qNewName "z" >>= (\n -> return (DVarT n, 0))
            lnames <- replicateM numArgs1 (qNewName "a")
            rnames <- replicateM numArgs2 (qNewName "b")
            let lvars = map DVarT lnames
                rvars = map DVarT rnames
                ltype = foldType (DConT name1) lvars
                rtype = foldType name2 rvars
            return $ DTySynEqn [ltype, rtype] (DConT result)
