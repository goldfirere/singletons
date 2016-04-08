{- Data/Singletons/Single/Eq.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Defines functions to generate SEq and SDecide instances.
-}

{-# LANGUAGE ViewPatterns, CPP #-}

module Data.Singletons.Single.Eq where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Singletons.Util
import Data.Singletons.Names
import Control.Monad

-- making the SEq instance and the SDecide instance are rather similar,
-- so we generalize
type EqualityClassDesc q = ((DCon, DCon) -> q DClause, Name, Name)
sEqClassDesc, sDecideClassDesc :: Quasi q => EqualityClassDesc q
sEqClassDesc = (mkEqMethClause, sEqClassName, sEqMethName)
sDecideClassDesc = (mkDecideMethClause, sDecideClassName, sDecideMethName)

-- pass the *singleton* constructors, not the originals
mkEqualityInstance :: Quasi q => DKind -> [DCon]
                   -> EqualityClassDesc q -> q DDec
mkEqualityInstance k ctors (mkMeth, className, methName) = do
  let ctorPairs = [ (c1, c2) | c1 <- ctors, c2 <- ctors ]
  methClauses <- if null ctors
                 then mkEmptyMethClauses
                 else mapM mkMeth ctorPairs
  return $ DInstanceD (map (\kvar -> (DConPr className) `DAppPr` kindParam kvar)
                           (getKindVars k))
                     (DAppT (DConT className)
                            (kindParam k))
                     [DLetDec $ DFunD methName methClauses]
  where getKindVars :: DKind -> [DKind]
#if MIN_VERSION_th_desugar(1,6,0)
        getKindVars (DVarT x)         = [DVarT x]
        getKindVars DStarT            = []
        getKindVars (unfoldArrowT -> Just (arg,res)) = concatMap getKindVars [arg, res]
        getKindVars (unfoldDConTApp -> Just (_,args)) = concatMap getKindVars args
#else
        getKindVars (DVarK x)         = [DVarK x]
        getKindVars (DConK _ args)    = concatMap getKindVars args
        getKindVars DStarK            = []
        getKindVars (DArrowK arg res) = concatMap getKindVars [arg, res]
#endif
        getKindVars other             =
          error ("getKindVars sees an unusual kind: " ++ show other)

        mkEmptyMethClauses :: Quasi q => q [DClause]
        mkEmptyMethClauses = do
          a <- qNewName "a"
          return [DClause [DVarPa a, DWildPa] (DCaseE (DVarE a) emptyMatches)]

mkEqMethClause :: Quasi q => (DCon, DCon) -> q DClause
mkEqMethClause (c1, c2)
  | lname == rname = do
    lnames <- replicateM lNumArgs (qNewName "a")
    rnames <- replicateM lNumArgs (qNewName "b")
    let lpats = map DVarPa lnames
        rpats = map DVarPa rnames
        lvars = map DVarE lnames
        rvars = map DVarE rnames
    return $ DClause
      [DConPa lname lpats, DConPa rname rpats]
      (allExp (zipWith (\l r -> foldExp (DVarE sEqMethName) [l, r])
                        lvars rvars))
  | otherwise =
    return $ DClause
      [DConPa lname (replicate lNumArgs DWildPa),
       DConPa rname (replicate rNumArgs DWildPa)]
      (DConE $ singDataConName falseName)
  where allExp :: [DExp] -> DExp
        allExp [] = DConE $ singDataConName trueName
        allExp [one] = one
        allExp (h:t) = DAppE (DAppE (DVarE $ singValName andName) h) (allExp t)

        (lname, lNumArgs) = extractNameArgs c1
        (rname, rNumArgs) = extractNameArgs c2

mkDecideMethClause :: Quasi q => (DCon, DCon) -> q DClause
mkDecideMethClause (c1, c2)
  | lname == rname =
    if lNumArgs == 0
    then return $ DClause [DConPa lname [], DConPa rname []]
                          (DAppE (DConE provedName) (DConE reflName))
    else do
      lnames <- replicateM lNumArgs (qNewName "a")
      rnames <- replicateM lNumArgs (qNewName "b")
      contra <- qNewName "contra"
      let lpats = map DVarPa lnames
          rpats = map DVarPa rnames
          lvars = map DVarE lnames
          rvars = map DVarE rnames
      refl <- qNewName "refl"
      return $ DClause
        [DConPa lname lpats, DConPa rname rpats]
        (DCaseE (mkTupleDExp $
                 zipWith (\l r -> foldExp (DVarE sDecideMethName) [l, r])
                         lvars rvars)
                ((DMatch (mkTupleDPat (replicate lNumArgs
                                        (DConPa provedName [DConPa reflName []])))
                        (DAppE (DConE provedName) (DConE reflName))) :
                 [DMatch (mkTupleDPat (replicate i DWildPa ++
                                       DConPa disprovedName [DVarPa contra] :
                                       replicate (lNumArgs - i - 1) DWildPa))
                         (DAppE (DConE disprovedName)
                                (DLamE [refl] $
                                 DCaseE (DVarE refl)
                                        [DMatch (DConPa reflName []) $
                                         (DAppE (DVarE contra)
                                                (DConE reflName))]))
                 | i <- [0..lNumArgs-1] ]))

  | otherwise = do
    x <- qNewName "x"
    return $ DClause
      [DConPa lname (replicate lNumArgs DWildPa),
       DConPa rname (replicate rNumArgs DWildPa)]
      (DAppE (DConE disprovedName) (DLamE [x] (DCaseE (DVarE x) emptyMatches)))

  where
    (lname, lNumArgs) = extractNameArgs c1
    (rname, rNumArgs) = extractNameArgs c2
