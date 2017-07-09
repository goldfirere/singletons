{- Data/Singletons/Promote/Eq.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

This module defines the functions that generate type-level equality type
family instances.
-}

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
  let null_branch  = catch_all_case trueName
      false_branch = catch_all_case falseName
      branches | null cons = [null_branch]
               | otherwise = true_branches ++ [false_branch]
      closedFam = DClosedTypeFamilyD (DTypeFamilyHead helperName
                                                        -- We opt to give explicit kinds for the tyvars
                                                        -- in the helper type family.
                                                        -- See Note [Promoted class method kinds]
                                                        -- in Data.Singletons.Promote.
                                                      [ DKindedTV aName kind
                                                      , DKindedTV bName kind ]
                                                      (DKindSig boolKi)
                                                      Nothing)
                                     branches
      eqInst = DTySynInstD tyEqName (DTySynEqn [DVarT aName, DVarT bName]
                                             (foldType (DConT helperName)
                                                       [DVarT aName, DVarT bName]))
      inst = DInstanceD Nothing [] ((DConT $ promoteClassName eqName) `DAppT`
                                    kind) [eqInst]

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
          return $ DTySynEqn [DSigT DWildCardT kind, DSigT DWildCardT kind]
                             (promoteValRhs falseName)

        tyAll :: [DType] -> DType -- "all" at the type level
        tyAll [] = (promoteValRhs trueName)
        tyAll [one] = one
        tyAll (h:t) = foldType (DConT $ promoteValNameLhs andName) [h, (tyAll t)]
           -- I could use the Apply nonsense here, but there's no reason to
