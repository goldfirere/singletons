{- Data/Singletons/TH/Single/Decide.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

Defines functions to generate SDecide instances, as well as TestEquality and
TestCoercion instances that leverage SDecide.
-}

module Data.Singletons.TH.Single.Decide where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Singletons.TH.Deriving.Infer
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Promote.Type
import Data.Singletons.TH.Util
import Control.Monad

-- Make an instance of SDecide.
mkDecideInstance :: OptionsMonad q => Maybe DCxt -> DType
                 -> [DCon] -- ^ The /original/ constructors (for inferring the instance context)
                 -> [DCon] -- ^ The /singletons/ constructors
                 -> q DDec
mkDecideInstance mb_ctxt data_ty ctors sctors = do
  let sctorPairs = [ (sc1, sc2) | sc1 <- sctors, sc2 <- sctors ]
  methClauses <- if null sctors
                 then (:[]) <$> mkEmptyDecideMethClause
                 else mapM mkDecideMethClause sctorPairs
  constraints <- inferConstraintsDef mb_ctxt (DConT sDecideClassName) data_ty ctors
  data_ki <- promoteType data_ty
  return $ DInstanceD Nothing Nothing
                     constraints
                     (DAppT (DConT sDecideClassName) data_ki)
                     [DLetDec $ DFunD sDecideMethName methClauses]

-- Make a boilerplate Eq instance for a singleton type, e.g.,
--
-- @
-- instance Eq (SExample (z :: Example a)) where
--   _ == _ = True
-- @
mkEqInstanceForSingleton :: OptionsMonad q
                         => DType
                         -> Name
                         -- ^ The name of the data type
                         -> q DDec
mkEqInstanceForSingleton data_ty data_name = do
  opts <- getOptions
  z <- qNewName "z"
  data_ki <- promoteType data_ty
  let sdata_name = singledDataTypeName opts data_name
  pure $ DInstanceD Nothing Nothing []
           (DAppT (DConT eqName) (DConT sdata_name `DAppT` DSigT (DVarT z) data_ki))
           [DLetDec $
            DFunD equalsName
                  [DClause [DWildP, DWildP] (DConE trueName)]]

data TestInstance = TestEquality
                  | TestCoercion

-- Make an instance of TestEquality or TestCoercion by leveraging SDecide.
mkTestInstance :: OptionsMonad q => Maybe DCxt -> DType
               -> Name   -- ^ The name of the data type
               -> [DCon] -- ^ The /original/ constructors (for inferring the instance context)
               -> TestInstance -> q DDec
mkTestInstance mb_ctxt data_ty data_name ctors ti = do
  opts <- getOptions
  constraints <- inferConstraintsDef mb_ctxt (DConT sDecideClassName) data_ty ctors
  data_ki <- promoteType data_ty
  pure $ DInstanceD Nothing Nothing
                    constraints
                    (DAppT (DConT tiClassName)
                           (DConT (singledDataTypeName opts data_name)
                             `DSigT` (DArrowT `DAppT` data_ki `DAppT` DConT typeKindName)))
                    [DLetDec $ DFunD tiMethName
                                     [DClause [] (DVarE tiDefaultName)]]
  where
    (tiClassName, tiMethName, tiDefaultName) =
      case ti of
        TestEquality -> (testEqualityClassName, testEqualityMethName, decideEqualityName)
        TestCoercion -> (testCoercionClassName, testCoercionMethName, decideCoercionName)

mkDecideMethClause :: Quasi q => (DCon, DCon) -> q DClause
mkDecideMethClause (c1, c2)
  | lname == rname =
    if lNumArgs == 0
    then return $ DClause [DConP lname [] [], DConP rname [] []]
                          (DAppE (DConE provedName) (DConE reflName))
    else do
      lnames <- replicateM lNumArgs (qNewName "a")
      rnames <- replicateM lNumArgs (qNewName "b")
      contra <- qNewName "contra"
      let lpats = map DVarP lnames
          rpats = map DVarP rnames
          lvars = map DVarE lnames
          rvars = map DVarE rnames
      return $ DClause
        [DConP lname [] lpats, DConP rname [] rpats]
        (dCasesE
          (zipWith (\l r -> foldExp (DVarE sDecideMethName) [l, r])
                   lvars rvars)
          ((DClause
              (replicate
                lNumArgs
                (DConP provedName [] [DConP reflName [] []]))
              (DAppE (DConE provedName) (DConE reflName))) :
           [ DClause
               (replicate i DWildP ++
                  DConP disprovedName [] [DVarP contra] :
                  replicate (lNumArgs - i - 1) DWildP)
               (DAppE
                  (DConE disprovedName)
                  (dLamCaseE
                     [DMatch (DConP reflName [] []) $
                      (DAppE (DVarE contra)
                             (DConE reflName))]))
           | i <- [0..lNumArgs-1] ]))

  | otherwise =
    return $ DClause
      [DConP lname [] (replicate lNumArgs DWildP),
       DConP rname [] (replicate rNumArgs DWildP)]
      (DAppE (DConE disprovedName) (dLamCaseE []))

  where
    (lname, lNumArgs) = extractNameArgs c1
    (rname, rNumArgs) = extractNameArgs c2

mkEmptyDecideMethClause :: Quasi q => q DClause
mkEmptyDecideMethClause = do
  x <- qNewName "x"
  pure $ DClause [DVarP x, DWildP]
       $ DConE provedName `DAppE` dCaseE (DVarE x) []
