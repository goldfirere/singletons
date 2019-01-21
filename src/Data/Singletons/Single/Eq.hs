{- Data/Singletons/Single/Eq.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

Defines functions to generate SEq and SDecide instances.
-}

module Data.Singletons.Single.Eq where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Singletons.Deriving.Infer
import Data.Singletons.Util
import Data.Singletons.Names
import Control.Monad

-- making the SEq instance and the SDecide instance are rather similar,
-- so we generalize
type EqualityClassDesc q = ((DCon, DCon) -> q DClause, q DClause, Name, Name)
sEqClassDesc, sDecideClassDesc :: Quasi q => EqualityClassDesc q
sEqClassDesc = (mkEqMethClause, mkEmptyEqMethClause, sEqClassName, sEqMethName)
sDecideClassDesc = (mkDecideMethClause, mkEmptyDecideMethClause, sDecideClassName, sDecideMethName)

mkEqualityInstance :: DsMonad q => Maybe DCxt -> DKind
                   -> [DCon] -- ^ The /original/ constructors (for inferring the instance context)
                   -> [DCon] -- ^ The /singletons/ constructors
                   -> EqualityClassDesc q -> q DDec
mkEqualityInstance mb_ctxt k ctors sctors (mkMeth, mkEmpty, className, methName) = do
  let sctorPairs = [ (sc1, sc2) | sc1 <- sctors, sc2 <- sctors ]
  methClauses <- if null sctors
                 then (:[]) <$> mkEmpty
                 else mapM mkMeth sctorPairs
  constraints <- inferConstraintsDef mb_ctxt (DConT className) k ctors
  return $ DInstanceD Nothing Nothing
                     constraints
                     (DAppT (DConT className) k)
                     [DLetDec $ DFunD methName methClauses]

data TestInstance = TestEquality
                  | TestCoercion

-- Make an instance of TestEquality or TestCoercion by leveraging SDecide.
mkTestInstance :: DsMonad q => Maybe DCxt -> DKind
               -> Name   -- ^ The name of the data type
               -> [DCon] -- ^ The /original/ constructors (for inferring the instance context)
               -> TestInstance -> q DDec
mkTestInstance mb_ctxt k data_name ctors ti = do
  constraints <- inferConstraintsDef mb_ctxt (DConT sDecideClassName) k ctors
  pure $ DInstanceD Nothing Nothing
                    constraints
                    (DAppT (DConT tiClassName)
                           (DConT (singTyConName data_name)
                             `DSigT` (DArrowT `DAppT` k `DAppT` DConT typeKindName)))
                    [DLetDec $ DFunD tiMethName
                                     [DClause [] (DVarE tiDefaultName)]]
  where
    (tiClassName, tiMethName, tiDefaultName) =
      case ti of
        TestEquality -> (testEqualityClassName, testEqualityMethName, decideEqualityName)
        TestCoercion -> (testCoercionClassName, testCoercionMethName, decideCoercionName)

mkEqMethClause :: Quasi q => (DCon, DCon) -> q DClause
mkEqMethClause (c1, c2)
  | lname == rname = do
    lnames <- replicateM lNumArgs (qNewName "a")
    rnames <- replicateM lNumArgs (qNewName "b")
    let lpats = map DVarP lnames
        rpats = map DVarP rnames
        lvars = map DVarE lnames
        rvars = map DVarE rnames
    return $ DClause
      [DConP lname lpats, DConP rname rpats]
      (allExp (zipWith (\l r -> foldExp (DVarE sEqMethName) [l, r])
                        lvars rvars))
  | otherwise =
    return $ DClause
      [DConP lname (replicate lNumArgs DWildP),
       DConP rname (replicate rNumArgs DWildP)]
      (DConE $ singDataConName falseName)
  where allExp :: [DExp] -> DExp
        allExp [] = DConE $ singDataConName trueName
        allExp [one] = one
        allExp (h:t) = DAppE (DAppE (DVarE $ singValName andName) h) (allExp t)

        (lname, lNumArgs) = extractNameArgs c1
        (rname, rNumArgs) = extractNameArgs c2

mkEmptyEqMethClause :: Applicative q => q DClause
mkEmptyEqMethClause =
  pure $ DClause [DWildP, DWildP]
       $ DConE strueName

mkDecideMethClause :: Quasi q => (DCon, DCon) -> q DClause
mkDecideMethClause (c1, c2)
  | lname == rname =
    if lNumArgs == 0
    then return $ DClause [DConP lname [], DConP rname []]
                          (DAppE (DConE provedName) (DConE reflName))
    else do
      lnames <- replicateM lNumArgs (qNewName "a")
      rnames <- replicateM lNumArgs (qNewName "b")
      contra <- qNewName "contra"
      let lpats = map DVarP lnames
          rpats = map DVarP rnames
          lvars = map DVarE lnames
          rvars = map DVarE rnames
      refl <- qNewName "refl"
      return $ DClause
        [DConP lname lpats, DConP rname rpats]
        (DCaseE (mkTupleDExp $
                 zipWith (\l r -> foldExp (DVarE sDecideMethName) [l, r])
                         lvars rvars)
                ((DMatch (mkTupleDPat (replicate lNumArgs
                                        (DConP provedName [DConP reflName []])))
                        (DAppE (DConE provedName) (DConE reflName))) :
                 [DMatch (mkTupleDPat (replicate i DWildP ++
                                       DConP disprovedName [DVarP contra] :
                                       replicate (lNumArgs - i - 1) DWildP))
                         (DAppE (DConE disprovedName)
                                (DLamE [refl] $
                                 DCaseE (DVarE refl)
                                        [DMatch (DConP reflName []) $
                                         (DAppE (DVarE contra)
                                                (DConE reflName))]))
                 | i <- [0..lNumArgs-1] ]))

  | otherwise = do
    x <- qNewName "x"
    return $ DClause
      [DConP lname (replicate lNumArgs DWildP),
       DConP rname (replicate rNumArgs DWildP)]
      (DAppE (DConE disprovedName) (DLamE [x] (DCaseE (DVarE x) [])))

  where
    (lname, lNumArgs) = extractNameArgs c1
    (rname, rNumArgs) = extractNameArgs c2

mkEmptyDecideMethClause :: Quasi q => q DClause
mkEmptyDecideMethClause = do
  x <- qNewName "x"
  pure $ DClause [DVarP x, DWildP]
       $ DConE provedName `DAppE` DCaseE (DVarE x) []
