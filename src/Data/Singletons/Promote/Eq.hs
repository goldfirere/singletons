{- Data/Singletons/Promote/Eq.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

This module defines the functions that generate type-level equality type
family instances.
-}

{-# LANGUAGE CPP #-}

module Data.Singletons.Promote.Eq where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Util
import Control.Monad

-- produce a closed type family helper and the instance
-- for (:==) over the given list of ctors
mkEqTypeInstance :: Quasi q => DKind -> [DCon] -> q [DDec]
mkEqTypeInstance kind cons = do
  helperName <- newUniqueName "Equals"
  aName <- qNewName "a"
  bName <- qNewName "b"
  true_branches <- mapM mk_branch cons
  false_branch  <- false_case
#if MIN_VERSION_th_desugar(1,6,0)
  let closedFam = DClosedTypeFamilyD (DTypeFamilyHead
                                        helperName
                                        [ DKindedTV aName kind
                                        , DKindedTV bName kind]
                                        (DKindSig boolKi)
                                        Nothing)
                                     (true_branches ++ [false_branch])
#else
  let closedFam = DClosedTypeFamilyD helperName
                                     [ DKindedTV aName kind
                                     , DKindedTV bName kind ]
                                     (Just boolKi)
                                     (true_branches ++ [false_branch])
#endif
      eqInst = DTySynInstD tyEqName (DTySynEqn [ DSigT (DVarT aName) kind
                                               , DSigT (DVarT bName) kind ]
                                             (foldType (DConT helperName)
                                                       [DVarT aName, DVarT bName]))
      inst = DInstanceD [] ((DConT $ promoteClassName eqName) `DAppT`
                            kindParam kind) [eqInst]

  return [closedFam, inst]

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
