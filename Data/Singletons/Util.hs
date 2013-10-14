{- Data/Singletons/Util.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains helper functions internal to the singletons package.
Users of the package should not need to consult this file.
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data.Singletons.Util (
  module Data.Singletons.Util,
  module Language.Haskell.TH.Desugar )
  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar ( reifyWithWarning, getDataD )
import Data.Char
import Data.Data
import Data.List
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
newUniqueName :: String -> Q Name
newUniqueName str = do
  n <- newName str
  return $ mkName $ show n

-- like mkName, but in the Data.Singletons module
mkSingName :: String -> Name
mkSingName = mkName . ("Data.Singletons." ++)

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

-- a monad transformer for writing a monoid alongside returning a Q
type QWithAux m = WriterT m Q

-- run a computation with an auxiliary monoid, discarding the monoid result
evalWithoutAux :: QWithAux m a -> Q a
evalWithoutAux = liftM fst . runWriterT

-- run a computation with an auxiliary monoid, returning only the monoid result
evalForAux :: QWithAux m a -> Q m
evalForAux = execWriterT

-- run a computation with an auxiliary monoid, return both the result
-- of the computation and the monoid result
evalForPair :: QWithAux m a -> Q (a, m)
evalForPair = runWriterT

-- in a computation with an auxiliary map, add a binding to the map
addBinding :: Ord k => k -> v -> QWithAux (Map.Map k v) ()
addBinding k v = tell (Map.singleton k v)

-- in a computation with an auxiliar list, add an element to the list
addElement :: elt -> QWithAux [elt] ()
addElement elt = tell [elt]

-- does a TH structure contain a name?
containsName :: Data a => Name -> a -> Bool
containsName n = everything (||) (mkQ False (== n))

-- lift concatMap into a monad
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM fn list = do
  bss <- mapM fn list
  return $ concat bss

