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

-- Why do we have two different versions of this code? Because GHC 7.6, which
-- doesn't allow any overlap among type family equations, needs O(n^2) instances.
-- Yuck. But, GHC 7.8 can get away with only O(n) equations in a closed type
-- family. The difference is significant enough to make it worth maintaining two
-- different generation functions, in RAE's opinion.
--
-- If we wish to change this, delete the 7.8 code -- the 7.6 code should work
-- just fine under 7.8.

#if __GLASGOW_HASKELL__ >= 707
-- produce a closed type family helper and the instance
-- for (:==) over the given list of ctors
mkEqTypeInstance :: DsMonad q => DKind -> [DCon] -> q [DDec]
mkEqTypeInstance kind cons = do
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
      inst = DInstanceD [] ((DConT $ promoteClassName eqName) `DAppT`
                            kindParam kind) [eqInst]
                                     
  return [closedFam, inst]

  where mk_branch :: DsMonad q => DCon -> q DTySynEqn
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

        false_case :: DsMonad q => q DTySynEqn
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

#else

-- produce the type instance for (:==) for the given pair of constructors
mkEqTypeInstance :: DsMonad q => (DCon, DCon) -> q DDec
mkEqTypeInstance (c1, c2) =
  if c1 == c2
  then do
    let (name, numArgs) = extractNameArgs c1
    lnames <- replicateM numArgs (qNewName "a")
    rnames <- replicateM numArgs (qNewName "b")
    let lvars = map DVarT lnames
        rvars = map DVarT rnames
    return $ DTySynInstD tyEqName $ DTySynEqn
      [foldType (DConT name) lvars,
       foldType (DConT name) rvars]
      (tyAll (zipWith (\l r -> foldType (DConT tyEqName) [l, r])
                      lvars rvars))
  else do
    let (lname, lNumArgs) = extractNameArgs c1
        (rname, rNumArgs) = extractNameArgs c2
    lnames <- replicateM lNumArgs (qNewName "a")
    rnames <- replicateM rNumArgs (qNewName "b")
    return $ DTySynInstD tyEqName $ DTySynEqn
      [foldType (DConT lname) (map DVarT lnames),
       foldType (DConT rname) (map DVarT rnames)]
      falseTySym
  where tyAll :: [DType] -> DType -- "all" at the type level
        tyAll [] = trueTySym
        tyAll [one] = one
        tyAll (h:t) = foldType (DConT $ promoteValNameLhs andName) [h, (tyAll t)]

#endif
