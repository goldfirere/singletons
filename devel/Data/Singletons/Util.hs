{- Data/Singletons/Util.hs

(c) Richard Eisenberg 2012
eir@cis.upenn.edu

This file contains helper functions internal to the singletons package.
Users of the package should not need to consult this file.
-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data.Singletons.Util where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char
import Data.Maybe
import Data.Data
import Data.List
import Control.Monad
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Generics

-- reify a declaration, warning the user about splices if the reify fails
reifyWithWarning :: Name -> Q Info
reifyWithWarning name = recover
  (fail $ "Looking up " ++ (show name) ++ " in the list of available " ++
        "declarations failed.\nThis lookup fails if the declaration " ++
        "referenced was made in same Template\nHaskell splice as the use " ++
        "of the declaration. If this is the case, put\nthe reference to " ++
        "the declaration in a new splice.")
  (reify name)

-- check if a string is the name of a tuple
isTupleString :: String -> Bool
isTupleString s =
  (length s > 1) &&
  (head s == '(') &&
  (last s == ')') &&
  ((length (takeWhile (== ',') (tail s))) == ((length s) - 2))

-- check if a name is a tuple name
isTupleName :: Name -> Bool
isTupleName = isTupleString . nameBase

-- extract the degree of a tuple
tupleDegree :: String -> Int
tupleDegree "()" = 0
tupleDegree s = length s - 1

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

-- extract the name from a TyVarBndr
extractTvbName :: TyVarBndr -> Name
extractTvbName (PlainTV n) = n
extractTvbName (KindedTV n _) = n

-- extract the kind from a TyVarBndr. Returns '*' by default.
extractTvbKind :: TyVarBndr -> Kind
extractTvbKind (PlainTV _) = StarT -- FIXME: This seems wrong.
extractTvbKind (KindedTV _ k) = k

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

