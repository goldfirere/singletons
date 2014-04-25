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

-- produce a closed type family helper and the instance
-- for (:==) over the given list of ctors
mkOrdTypeInstance :: Quasi q => DKind -> [DCon] -> q [DDec]
mkOrdTypeInstance kind cons = undefined
{-
  helperName <- newUniqueName "Equals"
  aName <- qNewName "a"
  bName <- qNewName "b"
  true_branches <- mapM mk_branch cons
  false_branch  <- false_case
  let closedFam = DClosedTypeFamilyD helperName
                                     [ DKindedTV aName kind
                                     , DKindedTV bName kind ]
                                     (Just boolKi)
                                     (true_branches ++ [false_branch])
      eqInst = DTySynInstD tyEqName (DTySynEqn [ DSigT (DVarT aName) kind
                                               , DSigT (DVarT bName) kind ]
                                             (foldType (DConT helperName)
                                                       [DVarT aName, DVarT bName]))
  return [closedFam, eqInst]

  where mk_branch :: Quasi q => DCon -> q DTySynEqn
        mk_branch con = do
          let (name, numArgs) = extractNameArgs con
          lnames <- replicateM numArgs (qNewName "a")
          rnames <- replicateM numArgs (qNewName "b")
          let lvars = map DVarT lnames
              rvars = map DVarT rnames
              ltype = foldType (DConT name) lvars
              rtype = foldType (DConT name) rvars
              results = zipWith (\l r -> foldType (DConT tyEqName) [l, r]) lvars rvars
              result = tyAll results
          return $ DTySynEqn [ltype, rtype] result

        false_case :: Quasi q => q DTySynEqn
        false_case = do
          lvar <- qNewName "a"
          rvar <- qNewName "b"
          return $ DTySynEqn [DSigT (DVarT lvar) kind, DSigT (DVarT rvar) kind]
                             (promoteValRhs falseName)

        tyAll :: [DType] -> DType -- "all" at the type level
        tyAll [] = (promoteValRhs trueName)
        tyAll [one] = one
        tyAll (h:t) = foldType (DConT $ promoteValNameLhs andName) [h, (tyAll t)]
           -- I could use the Apply nonsense here, but there's no reason to

-}
