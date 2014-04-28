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
  (compareEqns, eqDecs) <- evalForPair $ mapM (mkCompareEqn half) consPairs
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
  return (closedFam : compareInst : eqDecs)

  where mkCompareEqn :: Quasi q => Int -> ((DCon, Int), (DCon, Int))
                                -> QWithAux [DDec] q DTySynEqn
        mkCompareEqn half ((con1, tag1), (con2, tag2))
            | tag1 > tag2 && tag1 <= half =
                mkCompareEqnHelper con1 (Just con2) gtT
            | tag1 < tag2 && tag1 >  half = do
                mkCompareEqnHelper con1 (Just con2) ltT
            | tag1 < tag2 && tag1 <= half =
                mkCompareEqnHelper con1 Nothing ltT
            | tag1 > tag2 && tag1 >  half =
                mkCompareEqnHelper con1 Nothing gtT
            | otherwise =
                mkCompareEqual con1

        eqT = DConT ordEQSymName
        ltT = DConT ordLTSymName
        gtT = DConT ordGTSymName

        mkCompareEqnHelper :: Quasi q => DCon -> Maybe DCon -> DType -> q DTySynEqn
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
            return $ DTySynEqn [ltype, rtype] result

        mkCompareEqual :: Quasi q => DCon -> QWithAux [DDec] q DTySynEqn
        mkCompareEqual con = do
            let (name, numArgs) = extractNameArgs con
            case numArgs of
              0 -> return $ DTySynEqn [DConT name, DConT name] eqT
              _ -> do
                helperName <- newUniqueName "OrderingEqualCase"
                buildHelperTyFam numArgs helperName

                -- Call the helper function
                lnames <- replicateM numArgs (qNewName "a")
                rnames <- replicateM numArgs (qNewName "b")
                let lvars      = map DVarT lnames
                    rvars      = map DVarT rnames
                    ltype      = foldType (DConT name) lvars
                    rtype      = foldType (DConT name) rvars
                    callParams = zipWith (\l r -> foldType (DConT tyCompareName) [l,r])
                                          lvars rvars
                    call = foldType (DConT helperName) callParams
                return $ DTySynEqn [ltype, rtype] call
            where
                  buildHelperTyFam :: Quasi q => Int -> Name -> QWithAux [DDec] q ()
                  buildHelperTyFam numArgs helperName = do
                    let orderingKCon = DConK orderingName []
                    -- Build helper type family that does the comparison
                    (patterns, results) <- buildEqnPats numArgs ([[]], [eqT])
                    tyFamParamNames <- replicateM numArgs (qNewName "a")
                    let eqns = map (uncurry DTySynEqn) (zip patterns results)
                        closedFam = DClosedTypeFamilyD helperName
                                      (zipWith DKindedTV tyFamParamNames
                                              (repeat orderingKCon))
                                      (Just orderingKCon)
                                      eqns
                    addElement closedFam
                    return ()

                  buildEqnPats :: Quasi q => Int -> ([[DType]], [DType])
                                          -> q ([[DType]], [DType])
                  buildEqnPats 0 acc = return acc
                  buildEqnPats n acc = do
                    let eqns    = fst acc
                        results = snd acc
                        eqnNo   = length (head eqns)
                        newEqs  = map (eqT :) eqns
                    names <- replicateM eqnNo (qNewName "a")
                    let tys   = map DVarT names
                        ltRow = ltT : tys
                        gtRow = gtT : tys
                    buildEqnPats (n-1) ( ltRow : gtRow : newEqs
                                       , ltT : gtT : results )
