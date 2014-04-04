{- Data/Singletons/Util.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains helper functions internal to the singletons package.
Users of the package should not need to consult this file.
-}

{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, RankNTypes,
             TemplateHaskell, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving,
             UndecidableInstances, MagicHash, UnboxedTuples #-}

module Data.Singletons.Util (
  module Data.Singletons.Util,
  module Language.Haskell.TH.Desugar )
  where

import Prelude hiding ( exp, foldl, concat, mapM, any )
import Language.Haskell.TH hiding ( Q )
import Language.Haskell.TH.Syntax ( Quasi(..) )
import Language.Haskell.TH.Desugar
import Data.Char
import Control.Monad hiding ( mapM )
import Control.Applicative
import Control.Monad.Writer hiding ( mapM )
import Control.Monad.Reader hiding ( mapM )
import qualified Data.Map as Map
import Data.Foldable
import Data.Traversable

-- The list of types that singletons processes by default
basicTypes :: [Name]
basicTypes = [ ''Bool
             , ''Maybe
             , ''Either
             , ''Ordering
             , ''[]
             , ''()
             , ''(,)
             , ''(,,)
             , ''(,,,)
             , ''(,,,,)
             , ''(,,,,,)
             , ''(,,,,,,)
             ]

-- like reportWarning, but generalized to any Quasi
qReportWarning :: Quasi q => String -> q ()
qReportWarning = qReport False

-- like reportError, but generalized to any Quasi
qReportError :: Quasi q => String -> q ()
qReportError = qReport True

checkForRep :: Quasi q => [Name] -> q ()
checkForRep names =
  when (any ((== "Rep") . nameBase) names)
    (fail $ "A data type named <<Rep>> is a special case.\n" ++
            "Promoting it will not work as expected.\n" ++
            "Please choose another name for your data type.")

checkForRepInDecls :: Quasi q => [DDec] -> q ()
checkForRepInDecls decls =
  checkForRep (allNamesIn decls)

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

-- extract the degree of an unboxed tuple
unboxedTupleDegree_maybe :: String -> Maybe Int
unboxedTupleDegree_maybe s = do
  '(' : '#' : s1 <- return s
  (commas, "#)") <- return $ span (== ',') s1
  let degree
        | "" <- commas = 0
        | otherwise    = length commas + 1
  return degree

-- extract the degree of a tuple name
unboxedTupleNameDegree_maybe :: Name -> Maybe Int
unboxedTupleNameDegree_maybe = unboxedTupleDegree_maybe . nameBase

tysOfConFields :: DConFields -> [DType]
tysOfConFields (DNormalC stys) = map snd stys
tysOfConFields (DRecC vstys)   = map (\(_,_,ty) -> ty) vstys

-- extract the name and number of arguments to a constructor
extractNameArgs :: DCon -> (Name, Int)
extractNameArgs = liftSnd length . extractNameTypes

-- extract the name and types of constructor arguments
extractNameTypes :: DCon -> (Name, [DType])
extractNameTypes (DCon _ _ n fields) = (n, tysOfConFields fields)

-- reinterpret a name. This is useful when a Name has an associated
-- namespace that we wish to forget
reinterpret :: Name -> Name
reinterpret = mkName . nameBase

{-# DEPRECATED reinterpret, catNames "" #-} -- RAE

catNames :: Name -> Name -> Name
catNames n1 n2 = mkName (nameBase n1 ++ nameBase n2)

-- is an identifier uppercase?
isUpcase :: Name -> Bool
isUpcase n = let first = head (nameBase n) in isUpper first || first == ':'

-- make an identifier uppercase
upcase :: Name -> Name
upcase = mkName . toUpcaseStr

-- make an identifier uppercase and return it as a String
toUpcaseStr :: Name -> String
toUpcaseStr n
  | isUpcase n
  = nameBase n

  | otherwise
  = let str   = nameBase n
        first = head str
    in if isLetter first
       then (toUpper first) : tail str
       else ':' : str

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
extractTvbKind :: DTyVarBndr -> Maybe DKind
extractTvbKind (DPlainTV _) = Nothing
extractTvbKind (DKindedTV _ k) = Just k

-- extract the name from a TyVarBndr.
extractTvbName :: DTyVarBndr -> Name
extractTvbName (DPlainTV n) = n
extractTvbName (DKindedTV n _) = n

-- use the kind provided, or make a fresh kind variable
inferKind :: Quasi q => Maybe DKind -> q DKind
inferKind (Just k) = return k
inferKind Nothing = do
  newK <- qNewName "k"
  return $ DVarK newK

-- apply a type to a list of types
foldType :: DType -> [DType] -> DType
foldType = foldl DAppT

-- apply an expression to a list of expressions
foldExp :: DExp -> [DExp] -> DExp
foldExp = foldl DAppE

-- is a kind a variable?
isVarK :: DKind -> Bool
isVarK (DVarK _) = True
isVarK _ = False

-- is a function type?
isFunTy :: DType -> Bool
isFunTy (DAppT (DAppT DArrowT _) _) = True
isFunTy (DForallT _ _ _)            = True
isFunTy _                           = False

-- choose the first non-empty list
orIfEmpty :: [a] -> [a] -> [a]
orIfEmpty [] x = x
orIfEmpty x  _ = x

-- an empty list of matches, compatible with GHC 7.6.3
emptyMatches :: [DMatch]
emptyMatches = [DMatch DWildPa (DAppE (DVarE 'error) (DLitE (StringL errStr)))]
  where errStr = "Empty case reached -- this should be impossible"

-- build a pattern match over several expressions, each with only one pattern
multiCase :: [DExp] -> [DPat] -> DExp -> DExp
multiCase [] [] body = body
multiCase scruts pats body =
  DCaseE (mkTupleDExp scruts) [DMatch (mkTupleDPat pats) body]

-- a monad transformer for writing a monoid alongside returning a Q
newtype QWithAux m q a = QWA { runQWA :: WriterT m q a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter m)

deriving instance (Monoid m, MonadReader r q) => MonadReader r (QWithAux m q)

-- make a Quasi instance for easy lifting
instance (Quasi q, Monoid m) => Quasi (QWithAux m q) where
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
  qReifyModule      = lift `comp1` qReifyModule
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
evalWithoutAux :: Quasi q => QWithAux m q a -> q a
evalWithoutAux = liftM fst . runWriterT . runQWA

-- run a computation with an auxiliary monoid, returning only the monoid result
evalForAux :: Quasi q => QWithAux m q a -> q m
evalForAux = execWriterT . runQWA

-- run a computation with an auxiliary monoid, return both the result
-- of the computation and the monoid result
evalForPair :: Quasi q => QWithAux m q a -> q (a, m)
evalForPair = runWriterT . runQWA

-- in a computation with an auxiliary map, add a binding to the map
addBinding :: (Quasi q, Ord k) => k -> v -> QWithAux (Map.Map k v) q ()
addBinding k v = tell (Map.singleton k v)

-- in a computation with an auxiliar list, add an element to the list
addElement :: Quasi q => elt -> QWithAux [elt] q ()
addElement elt = tell [elt]

-- lift concatMap into a monad
-- could this be more efficient?
concatMapM :: (Monad monad, Monoid monoid, Traversable traversable)
           => (a -> monad monoid) -> traversable a -> monad monoid
concatMapM fn list = do
  bss <- mapM fn list
  return $ fold bss

-- make a one-element list
listify :: a -> [a]
listify = (:[])

liftSnd :: (a -> b) -> (c, a) -> (c, b)
liftSnd f (c, a) = (c, f a)

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = go [] []
  where go bs cs []     = (reverse bs, reverse cs)
        go bs cs (a:as) =
          case f a of
            Left b  -> go (b:bs) cs as
            Right c -> go bs (c:cs) as

