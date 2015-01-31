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

mkOrdTypeInstance :: DsMonad q => DKind -> [DCon] -> q [DDec]
mkOrdTypeInstance kind cons = do
  let tagged_cons = zip cons [1..]
      con_pairs   = [ (c1, c2) | c1 <- tagged_cons, c2 <- tagged_cons ]
  eqns <- mapM mkOrdTySynEqn con_pairs
  let tyfam_insts = map (DTySynInstD tyCompareName) eqns
      pord_name   = promoteClassName ordName
      pord_inst   = DInstanceD [] (DConT pord_name `DAppT` kindParam kind)
                               tyfam_insts
  return [pord_inst]

mkOrdTySynEqn :: DsMonad q => ((DCon, Int), (DCon, Int)) -> q DTySynEqn
mkOrdTySynEqn ((c1, n1), (c2, n2)) = do
  let DCon _tvbs1 _cxt1 con_name1 con_fields1 = c1
      DCon _tvbs2 _cxt2 con_name2 con_fields2 = c2
  lhs_names <- mapM (const $ qNewName "lhs") (tysOfConFields con_fields1)
  rhs_names <- mapM (const $ qNewName "rhs") (tysOfConFields con_fields2)
  let lhs_ty = foldType (DConT con_name1) (map DVarT lhs_names)
      rhs_ty = foldType (DConT con_name2) (map DVarT rhs_names)
      result = case n1 `compare` n2 of
        EQ -> let cmps   = zipWith (\lhs rhs ->
                                     foldType (DConT tyCompareName) [ DVarT lhs
                                                                    , DVarT rhs ])
                           lhs_names rhs_names
              in
              foldl (\l r -> foldType (DConT tyThenCmpName) [l, r])
                    (DConT 'EQ) cmps

        LT -> DConT 'LT
        GT -> DConT 'GT
  return $ DTySynEqn [lhs_ty, rhs_ty] result

{-
-- Note [Deriving Ord]
-- ~~~~~~~~~~~~~~~~~~~
--
-- We derive instances of Ord by generating promoted instance of Compare.  Under
-- GHC 7.8 this is done by generating a closed type family that does tha
-- comparing for given datatype and then making appropriate instance of Compare
-- open type family. There are two interesting points in this
-- algorithm. Firstly we minimize the number of equations required to compare
-- all existing data constructors. To do this we use a catch-all equations. For
-- example for this data type:
--
--  data Foo = A | B | C | D | E | F deriving (Eq,Ord)
--
-- We generate equations:
--
--  CompareFoo A A = EQ
--  CompareFoo A a = LT -- catch-all case
--  CompareFoo B A = GT
--  CompareFoo B B = EQ
--  CompareFoo B a = LT -- catch-all case
--
-- This however would be very inefficient for the last constructor:
--
--  CompareFoo F A = GT
--  CompareFoo F B = GT
--  CompareFoo F C = GT
--  CompareFoo F D = GT
--  CompareFoo F E = GT
--  CompareFoo F F = EQ
--
-- So once we get past half of the constructors we reverse the order in which we
-- test second constructor passed to Compare:
--
--  CompareFoo F F = EQ
--  CompareFoo F a = GT
--  CompareFoo E F = LT
--  CompareFoo E E = EQ
--  CompareFoo E a = GT
--
-- Second interesting point in our algorithm is comparing identical
-- constructors. Obviously if they store no data they are equal. But if
-- constructor has any fields then they must be compared by calling Compare on
-- every field until we get LT or GT result. To do this we generate a helper
-- type function that does all the comparing. For example (,,) constructor has
-- three fields and we generate this code:
--
-- type family OrderingEqualCase (t1 :: Ordering)
--                               (t2 :: Ordering)
--                               (t3 :: Ordering) :: Ordering where
--   OrderingEqualCase LTSym0 a      b      = LTSym0
--   OrderingEqualCase GTSym0 a      b      = GTSym0
--   OrderingEqualCase EQSym0 LTSym0 b      = LTSym0
--   OrderingEqualCase EQSym0 GTSym0 b      = GTSym0
--   OrderingEqualCase EQSym0 EQSym0 LTSym0 = LTSym0
--   OrderingEqualCase EQSym0 EQSym0 GTSym0 = GTSym0
--   OrderingEqualCase EQSym0 EQSym0 EQSym0 = EQSym0
--
--  type family Compare_helper (a :: (k1,k2,k3)) (b :: (k1,k2,k3) :: Ordering where
--    Compare_helper (a1,a2,a3) (b1,b2,b3) =
--      OrderingEqualCase (Compare a1 b1) (Compare a2 b2) (Compare a3 b3)
--
---- Notice that we perform only necessary comparisons. If we can determine
---- ordering based on comparing first field we ignore the remaining fields
---- (although this implementation requires that we actually compare all fields
---- at the call site).

mkOrdTypeInstance :: DsMonad q => DKind -> [DCon] -> q [DDec]
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

  where mkCompareEqn :: DsMonad q => Int -> ((DCon, Int), (DCon, Int))
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

        mkCompareEqnHelper :: DsMonad q => DCon -> Maybe DCon -> DType -> q DTySynEqn
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

        mkCompareEqual :: DsMonad q => DCon -> QWithAux [DDec] q DTySynEqn
        mkCompareEqual con = do
            let (name, numArgs) = extractNameArgs con
            case numArgs of
              -- If constructor has no fields it is equal to itself
              0 -> return $ DTySynEqn [DConT name, DConT name] eqT
              -- But if it has fields we have to compare them one by one
              _ -> do
                helperName <- newUniqueName "OrderingEqualCase"
                -- Build helper type family that does the comparison
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
                  buildHelperTyFam :: DsMonad q => Int -> Name -> QWithAux [DDec] q ()
                  buildHelperTyFam numArgs helperName = do
                    let orderingKCon = DConK orderingName []
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

                  buildEqnPats :: DsMonad q => Int -> ([[DType]], [DType])
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

-}
