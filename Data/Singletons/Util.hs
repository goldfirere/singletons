{- Data/Singletons/Util.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains helper functions internal to the singletons package.
Users of the package should not need to consult this file.
-}

{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, RankNTypes,
             PatternGuards, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data.Singletons.Util (
  module Data.Singletons.Util,
  module Language.Haskell.TH.Desugar )
  where

import Language.Haskell.TH hiding ( Q )
import Language.Haskell.TH.Syntax ( Quasi(..) )
import Language.Haskell.TH.Desugar ( reifyWithWarning, getDataD )
import Data.Char
import Data.Data
import Control.Monad
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Generics

mkTyFamInst :: Name -> [Type] -> Type -> Dec
mkTyFamInst name lhs rhs =
#if __GLASGOW_HASKELL__ >= 707
  TySynInstD name (TySynEqn lhs rhs)
#else
  TySynInstD name lhs rhs
#endif

-- like newName, but even more unique (unique across different splices)
-- TH doesn't allow "newName"s to work at the top-level, so we have to
-- do this trick to ensure the Extract functions are unique
newUniqueName :: Quasi q => String -> q Name
newUniqueName str = do
  n <- qNewName str
  return $ mkName $ show n

-- like reportWarning, but generalized to any Quasi
qReportWarning :: Quasi q => String -> q ()
qReportWarning = qReport False

-- extract the degree of a tuple
tupleDegree_maybe :: String -> Maybe Int
tupleDegree_maybe s = do
  '(' : s1 <- return s 
  (commas, ")") <- return $ span (== ',') s1
  let degree
        | "" <- commas = 0
        | otherwise    = length commas + 1
  return degree

-- extract the degree of a tuple name
tupleNameDegree_maybe :: Name -> Maybe Int
tupleNameDegree_maybe = tupleDegree_maybe . nameBase

-- reduce the four cases of a 'Con' to just two: monomorphic and polymorphic
-- and convert 'StrictType' to 'Type'
ctorCases :: (Name -> [Type] -> a) -> ([TyVarBndr] -> Cxt -> Con -> a) -> Con -> a
ctorCases genFun forallFun ctor = case ctor of
  NormalC name stypes -> genFun name (map snd stypes)
  RecC name vstypes -> genFun name (map (\(_,_,ty) -> ty) vstypes)
  InfixC (_,ty1) name (_,ty2) -> genFun name [ty1, ty2]
  ForallC [] [] ctor' -> ctorCases genFun forallFun ctor'
  ForallC tvbs cx ctor' -> forallFun tvbs cx ctor' 

-- reduce the four cases of a 'Con' to just 1: a polymorphic Con is treated
-- as a monomorphic one
ctor1Case :: (Name -> [Type] -> a) -> Con -> a
ctor1Case mono = ctorCases mono (\_ _ ctor -> ctor1Case mono ctor)

-- extract the name and number of arguments to a constructor
extractNameArgs :: Con -> (Name, Int)
extractNameArgs = ctor1Case (\name tys -> (name, length tys))

-- reinterpret a name. This is useful when a Name has an associated
-- namespace that we wish to forget
reinterpret :: Name -> Name
reinterpret = mkName . nameBase

-- is an identifier uppercase?
isUpcase :: Name -> Bool
isUpcase n = let first = head (nameBase n) in isUpper first || first == ':'

-- make an identifier uppercase
upcase :: Name -> Name
upcase n =
  let str = nameBase n 
      first = head str in
    if isLetter first
     then mkName ((toUpper first) : tail str)
     else mkName (':' : str)

-- make an identifier lowercase
locase :: Name -> Name
locase n =
  let str = nameBase n
      first = head str in
    if isLetter first
     then mkName ((toLower first) : tail str)
     else mkName (tail str) -- remove the ":"

-- put an uppercase prefix on a name. Takes two prefixes: one for identifiers
-- and one for symbols
prefixUCName :: String -> String -> Name -> Name
prefixUCName pre tyPre n = case (nameBase n) of
    (':' : rest) -> mkName (tyPre ++ rest)
    alpha -> mkName (pre ++ alpha)

-- put a lowercase prefix on a name. Takes two prefixes: one for identifiers
-- and one for symbols
prefixLCName :: String -> String -> Name -> Name
prefixLCName pre tyPre n =
  let str = nameBase n
      first = head str in
    if isLetter first
     then mkName (pre ++ str)
     else mkName (tyPre ++ str)

-- extract the kind from a TyVarBndr. Returns '*' by default.
extractTvbKind :: TyVarBndr -> Kind
extractTvbKind (PlainTV _) = StarT -- FIXME: This seems wrong.
extractTvbKind (KindedTV _ k) = k

-- extract the name from a TyVarBndr.
extractTvbName :: TyVarBndr -> Name
extractTvbName (PlainTV n) = n
extractTvbName (KindedTV n _) = n

-- apply a type to a list of types
foldType :: Type -> [Type] -> Type
foldType = foldl AppT

-- apply an expression to a list of expressions
foldExp :: Exp -> [Exp] -> Exp
foldExp = foldl AppE

-- is a kind a variable?
isVarK :: Kind -> Bool
isVarK (VarT _) = True
isVarK _ = False

-- tuple up a list of expressions
mkTupleExp :: [Exp] -> Exp
mkTupleExp [x] = x
mkTupleExp xs  = TupE xs

-- tuple up a list of patterns
mkTuplePat :: [Pat] -> Pat
mkTuplePat [x] = x
mkTuplePat xs  = TupP xs

-- choose the first non-empty list
orIfEmpty :: [a] -> [a] -> [a]
orIfEmpty [] x = x
orIfEmpty x  _ = x

-- an empty list of matches, compatible with GHC 7.6.3
emptyMatches :: [Match]
#if __GLASGOW_HASKELL__ >= 707
emptyMatches = []
#else
emptyMatches = [Match WildP (NormalB (AppE (VarE 'error) (LitE (StringL errStr)))) []]
  where errStr = "Empty case reached -- this should be impossible"
#endif

-- build a pattern match over several expressions, each with only one pattern
multiCase :: [Exp] -> [Pat] -> Exp -> Exp
multiCase [] [] body = body
multiCase scruts pats body =
  CaseE (mkTupleExp scruts)
        [Match (mkTuplePat pats) (NormalB body) []]

-- a monad transformer for writing a monoid alongside returning a Q
type QWithAux q m = WriterT m q

-- make a Quasi instance for easy lifting
instance (Quasi q, Monoid m) => Quasi (QWithAux q m) where
  qNewName          = lift `comp1` qNewName
  qReport           = lift `comp2` qReport
  qLookupName       = lift `comp2` qLookupName
  qReify            = lift `comp1` qReify
  qReifyInstances   = lift `comp2` qReifyInstances
  qLocation         = lift qLocation
  qRunIO            = lift `comp1` qRunIO
  qAddDependentFile = lift `comp1` qAddDependentFile
#if __GLASGOW_HASKELL__ >= 707
  qReifyRoles       = lift `comp1` qReifyRoles
  qReifyAnnotations = lift `comp1` qReifyAnnotations
  qAddTopDecls      = lift `comp1` qAddTopDecls
  qAddModFinalizer  = lift `comp1` qAddModFinalizer
  qGetQ             = lift qGetQ
  qPutQ             = lift `comp1` qPutQ
#endif                      
  
  qRecover exp handler = do
    (result, aux) <- lift $ qRecover (evalForPair exp) (evalForPair handler)
    tell aux
    return result

-- helper functions for composition
comp1 :: (b -> c) -> (a -> b) -> a -> c
comp1 = (.)

comp2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
comp2 f g a b = f (g a b)

-- run a computation with an auxiliary monoid, discarding the monoid result
evalWithoutAux :: Quasi q => QWithAux q m a -> q a
evalWithoutAux = liftM fst . runWriterT

-- run a computation with an auxiliary monoid, returning only the monoid result
evalForAux :: Quasi q => QWithAux q m a -> q m
evalForAux = execWriterT

-- run a computation with an auxiliary monoid, return both the result
-- of the computation and the monoid result
evalForPair :: Quasi q => QWithAux q m a -> q (a, m)
evalForPair = runWriterT

-- in a computation with an auxiliary map, add a binding to the map
addBinding :: (Quasi q, Ord k) => k -> v -> QWithAux q (Map.Map k v) ()
addBinding k v = tell (Map.singleton k v)

-- in a computation with an auxiliar list, add an element to the list
addElement :: Quasi q => elt -> QWithAux q [elt] ()
addElement elt = tell [elt]

-- does a TH structure contain a name?
containsName :: Data a => Name -> a -> Bool
containsName n = everything (||) (mkQ False (== n))

-- lift concatMap into a monad
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM fn list = do
  bss <- mapM fn list
  return $ concat bss

-- make a one-element list
listify :: a -> [a]
listify = return