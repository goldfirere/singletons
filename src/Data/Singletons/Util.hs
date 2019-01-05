{- Data/Singletons/Util.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains helper functions internal to the singletons package.
Users of the package should not need to consult this file.
-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes,
             TemplateHaskell, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving,
             UndecidableInstances, MagicHash, UnboxedTuples,
             LambdaCase, NoMonomorphismRestriction #-}

module Data.Singletons.Util where

import Prelude hiding ( exp, foldl, concat, mapM, any, pred )
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import Data.Char
import Control.Monad hiding ( mapM )
import Control.Monad.Writer hiding ( mapM )
import Control.Monad.Reader hiding ( mapM )
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map ( Map )
import qualified Data.Monoid as Monoid
import Data.Semigroup as Semigroup
import Data.Foldable
import Data.Functor.Identity
import Data.Traversable
import Data.Generics
import Data.Maybe
import Data.Void
import Control.Monad.Fail ( MonadFail )

-- The list of types that singletons processes by default
basicTypes :: [Name]
basicTypes = [ ''Maybe
             , ''[]
             , ''Either
             , ''NonEmpty
             , ''Void
             ] ++ boundedBasicTypes

boundedBasicTypes :: [Name]
boundedBasicTypes =
            [ ''(,)
            , ''(,,)
            , ''(,,,)
            , ''(,,,,)
            , ''(,,,,,)
            , ''(,,,,,,)
            , ''Identity
            ] ++ enumBasicTypes

enumBasicTypes :: [Name]
enumBasicTypes = [ ''Bool, ''Ordering, ''() ]

semigroupBasicTypes :: [Name]
semigroupBasicTypes
  = [ ''Dual
    , ''All
    , ''Any
    , ''Sum
    , ''Product
    -- , ''Endo      see https://github.com/goldfirere/singletons/issues/82
    {- , ''Alt       singletons doesn't support higher kinds :(
                     see https://github.com/goldfirere/singletons/issues/150
    -}

    , ''Min
    , ''Max
    , ''Semigroup.First
    , ''Semigroup.Last
    , ''WrappedMonoid
    ]

monoidBasicTypes :: [Name]
monoidBasicTypes
  = [ ''Monoid.First
    , ''Monoid.Last
    ]

-- like reportWarning, but generalized to any Quasi
qReportWarning :: Quasi q => String -> q ()
qReportWarning = qReport False

-- like reportError, but generalized to any Quasi
qReportError :: Quasi q => String -> q ()
qReportError = qReport True

-- | Generate a new Unique
qNewUnique :: DsMonad q => q Int
qNewUnique = do
  Name _ flav <- qNewName "x"
  case flav of
    NameU n -> return n
    _       -> error "Internal error: `qNewName` didn't return a NameU"

checkForRep :: Quasi q => [Name] -> q ()
checkForRep names =
  when (any ((== "Rep") . nameBase) names)
    (fail $ "A data type named <<Rep>> is a special case.\n" ++
            "Promoting it will not work as expected.\n" ++
            "Please choose another name for your data type.")

checkForRepInDecls :: Quasi q => [DDec] -> q ()
checkForRepInDecls decls =
  checkForRep (allNamesIn decls)

tysOfConFields :: DConFields -> [DType]
tysOfConFields (DNormalC _ stys) = map snd stys
tysOfConFields (DRecC vstys)   = map (\(_,_,ty) -> ty) vstys

-- extract the name and number of arguments to a constructor
extractNameArgs :: DCon -> (Name, Int)
extractNameArgs = liftSnd length . extractNameTypes

-- extract the name and types of constructor arguments
extractNameTypes :: DCon -> (Name, [DType])
extractNameTypes (DCon _ _ n fields _) = (n, tysOfConFields fields)

extractName :: DCon -> Name
extractName (DCon _ _ n _ _) = n

-- | is a valid Haskell infix data constructor (i.e., does it begin with a colon?)
isInfixDataCon :: String -> Bool
isInfixDataCon (':':_) = True
isInfixDataCon _       = False

-- | Is an identifier a legal data constructor name in Haskell? That is, is its
-- first character an uppercase letter (prefix) or a colon (infix)?
isDataConName :: Name -> Bool
isDataConName n = let first = head (nameBase n) in isUpper first || first == ':'

-- | Is an identifier uppercase?
--
-- Note that this will always return 'False' for infix names, since the concept
-- of upper- and lower-case doesn't make sense for non-alphabetic characters.
-- If you want to check if a name is legal as a data constructor, use the
-- 'isDataConName' function.
isUpcase :: Name -> Bool
isUpcase n = let first = head (nameBase n) in isUpper first

-- Make an identifier uppercase. If the identifier is infix, this acts as the
-- identity function.
upcase :: Name -> Name
upcase = mkName . toUpcaseStr noPrefix

-- make an identifier uppercase and return it as a String
toUpcaseStr :: (String, String)  -- (alpha, symb) prefixes to prepend
            -> Name -> String
toUpcaseStr (alpha, symb) n
  | isHsLetter first
  = upcase_alpha

  | otherwise
  = upcase_symb

  where
    str   = nameBase n
    first = head str

    upcase_alpha = alpha ++ (toUpper first) : tail str
    upcase_symb = symb ++ str

noPrefix :: (String, String)
noPrefix = ("", "")

-- Put an uppercase prefix on a constructor name. Takes two prefixes:
-- one for identifiers and one for symbols.
--
-- This is different from 'prefixName' in that infix constructor names always
-- start with a colon, so we must insert the prefix after the colon in order
-- for the new name to be syntactically valid.
prefixConName :: String -> String -> Name -> Name
prefixConName pre tyPre n = case (nameBase n) of
    (':' : rest) -> mkName (':' : tyPre ++ rest)
    alpha -> mkName (pre ++ alpha)

-- Put a prefix on a name. Takes two prefixes: one for identifiers
-- and one for symbols.
prefixName :: String -> String -> Name -> Name
prefixName pre tyPre n =
  let str = nameBase n
      first = head str in
    if isHsLetter first
     then mkName (pre ++ str)
     else mkName (tyPre ++ str)

-- Put a suffix on a name. Takes two suffixes: one for identifiers
-- and one for symbols.
suffixName :: String -> String -> Name -> Name
suffixName ident symb n =
  let str = nameBase n
      first = head str in
  if isHsLetter first
  then mkName (str ++ ident)
  else mkName (str ++ symb)

-- convert a number into both alphanumeric and symoblic forms
uniquePrefixes :: String   -- alphanumeric prefix
               -> String   -- symbolic prefix
               -> Int
               -> (String, String)  -- (alphanum, symbolic)
uniquePrefixes alpha symb n = (alpha ++ n_str, symb ++ convert n_str)
  where
    n_str = show n

    convert [] = []
    convert (d : ds) =
      let d' = case d of
                 '0' -> '!'
                 '1' -> '#'
                 '2' -> '$'
                 '3' -> '%'
                 '4' -> '&'
                 '5' -> '*'
                 '6' -> '+'
                 '7' -> '.'
                 '8' -> '/'
                 '9' -> '>'
                 _   -> error "non-digit in show #"
      in d' : convert ds

-- extract the kind from a TyVarBndr
extractTvbKind :: DTyVarBndr -> Maybe DKind
extractTvbKind (DPlainTV _) = Nothing
extractTvbKind (DKindedTV _ k) = Just k

-- extract the name from a TyVarBndr.
extractTvbName :: DTyVarBndr -> Name
extractTvbName (DPlainTV n) = n
extractTvbName (DKindedTV n _) = n

tvbToType :: DTyVarBndr -> DType
tvbToType = DVarT . extractTvbName

inferMaybeKindTV :: Name -> Maybe DKind -> DTyVarBndr
inferMaybeKindTV n Nothing =  DPlainTV n
inferMaybeKindTV n (Just k) = DKindedTV n k

resultSigToMaybeKind :: DFamilyResultSig -> Maybe DKind
resultSigToMaybeKind DNoSig                      = Nothing
resultSigToMaybeKind (DKindSig k)                = Just k
resultSigToMaybeKind (DTyVarSig (DPlainTV _))    = Nothing
resultSigToMaybeKind (DTyVarSig (DKindedTV _ k)) = Just k

-- Reconstruct arrow kind from the list of kinds
ravel :: [DType] -> DType -> DType
ravel []    res  = res
ravel (h:t) res = DAppT (DAppT DArrowT h) (ravel t res)

-- count the number of arguments in a type
countArgs :: DType -> Int
countArgs ty = length args
  where (_, _, args, _) = unravel ty

-- changes all TyVars not to be NameU's. Workaround for GHC#11812
noExactTyVars :: Data a => a -> a
noExactTyVars = everywhere go
  where
    go :: Data a => a -> a
    go = mkT fix_tvb `extT` fix_ty `extT` fix_inj_ann

    no_exact_name :: Name -> Name
    no_exact_name (Name (OccName occ) (NameU unique)) = mkName (occ ++ show unique)
    no_exact_name n                                   = n

    fix_tvb (DPlainTV n)    = DPlainTV (no_exact_name n)
    fix_tvb (DKindedTV n k) = DKindedTV (no_exact_name n) k

    fix_ty (DVarT n)           = DVarT (no_exact_name n)
    fix_ty ty                  = ty

    fix_inj_ann (InjectivityAnn lhs rhs)
      = InjectivityAnn (no_exact_name lhs) (map no_exact_name rhs)

substKind :: Map Name DKind -> DKind -> DKind
substKind = substType

-- | Non–capture-avoiding substitution. (If you want capture-avoiding
-- substitution, use @substTy@ from "Language.Haskell.TH.Desugar.Subst".
substType :: Map Name DType -> DType -> DType
substType subst ty | Map.null subst = ty
substType subst (DForallT tvbs cxt inner_ty)
  = DForallT tvbs' cxt' inner_ty'
  where
    (subst', tvbs') = mapAccumL subst_tvb subst tvbs
    cxt'            = map (substType subst') cxt
    inner_ty'       = substType subst' inner_ty
substType subst (DAppT ty1 ty2) = substType subst ty1 `DAppT` substType subst ty2
substType subst (DSigT ty ki) = substType subst ty `DSigT` substType subst ki
substType subst (DVarT n) =
  case Map.lookup n subst of
    Just ki -> ki
    Nothing -> DVarT n
substType _ ty@(DConT {}) = ty
substType _ ty@(DArrowT)  = ty
substType _ ty@(DLitT {}) = ty
substType _ ty@DWildCardT = ty

subst_tvb :: Map Name DKind -> DTyVarBndr -> (Map Name DKind, DTyVarBndr)
subst_tvb s tvb@(DPlainTV n) = (Map.delete n s, tvb)
subst_tvb s (DKindedTV n k)  = (Map.delete n s, DKindedTV n (substKind s k))

cuskify :: DTyVarBndr -> DTyVarBndr
cuskify (DPlainTV tvname) = DKindedTV tvname $ DConT typeKindName
cuskify tvb               = tvb

-- apply a type to a list of types
foldType :: DType -> [DType] -> DType
foldType = foldl DAppT

-- apply a type to a list of type variable binders
foldTypeTvbs :: DType -> [DTyVarBndr] -> DType
foldTypeTvbs ty = foldType ty . map tvbToType

-- | Decompose an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would be unfolded to this:
--
-- @
-- Either :| [Int, Char]
-- @
unfoldType :: DType -> NonEmpty DType
unfoldType = go []
  where
    go :: [DType] -> DType -> NonEmpty DType
    go acc (DAppT t1 t2)    = go (t2:acc) t1
    go acc (DSigT t _)      = go acc t
    go acc (DForallT _ _ t) = go acc t
    go acc t                = t :| acc

-- Construct a data type's variable binders, possibly using fresh variables
-- from the data type's kind signature.
buildDataDTvbs :: DsMonad q => [DTyVarBndr] -> Maybe DKind -> q [DTyVarBndr]
buildDataDTvbs tvbs mk = do
  extra_tvbs <- mkExtraDKindBinders $ fromMaybe (DConT typeKindName) mk
  pure $ tvbs ++ extra_tvbs

-- apply an expression to a list of expressions
foldExp :: DExp -> [DExp] -> DExp
foldExp = foldl DAppE

-- is a function type?
isFunTy :: DType -> Bool
isFunTy (DAppT (DAppT DArrowT _) _) = True
isFunTy (DForallT _ _ _)            = True
isFunTy _                           = False

-- choose the first non-empty list
orIfEmpty :: [a] -> [a] -> [a]
orIfEmpty [] x = x
orIfEmpty x  _ = x

-- build a pattern match over several expressions, each with only one pattern
multiCase :: [DExp] -> [DPat] -> DExp -> DExp
multiCase [] [] body = body
multiCase scruts pats body =
  DCaseE (mkTupleDExp scruts) [DMatch (mkTupleDPat pats) body]

-- Make a desugar function into a TH function.
wrapDesugar :: (Desugar th ds, DsMonad q) => (th -> ds -> q ds) -> th -> q th
wrapDesugar f th = do
  ds <- desugar th
  fmap sweeten $ f th ds

-- a monad transformer for writing a monoid alongside returning a Q
newtype QWithAux m q a = QWA { runQWA :: WriterT m q a }
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadWriter m, MonadReader r
           , MonadFail, MonadIO )

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
  qReifyRoles       = lift `comp1` qReifyRoles
  qReifyAnnotations = lift `comp1` qReifyAnnotations
  qReifyModule      = lift `comp1` qReifyModule
  qAddTopDecls      = lift `comp1` qAddTopDecls
  qAddModFinalizer  = lift `comp1` qAddModFinalizer
  qGetQ             = lift qGetQ
  qPutQ             = lift `comp1` qPutQ

  qReifyFixity        = lift `comp1` qReifyFixity
  qReifyConStrictness = lift `comp1` qReifyConStrictness
  qIsExtEnabled       = lift `comp1` qIsExtEnabled
  qExtsEnabled        = lift qExtsEnabled
  qAddForeignFilePath = lift `comp2` qAddForeignFilePath
  qAddTempFile        = lift `comp1` qAddTempFile
  qAddCorePlugin      = lift `comp1` qAddCorePlugin

  qRecover exp handler = do
    (result, aux) <- lift $ qRecover (evalForPair exp) (evalForPair handler)
    tell aux
    return result

instance (DsMonad q, Monoid m) => DsMonad (QWithAux m q) where
  localDeclarations = lift localDeclarations

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
evalForPair :: QWithAux m q a -> q (a, m)
evalForPair = runWriterT . runQWA

-- in a computation with an auxiliary map, add a binding to the map
addBinding :: (Quasi q, Ord k) => k -> v -> QWithAux (Map.Map k v) q ()
addBinding k v = tell (Map.singleton k v)

-- in a computation with an auxiliar list, add an element to the list
addElement :: Quasi q => elt -> QWithAux [elt] q ()
addElement elt = tell [elt]

-- | Call 'lookupTypeNameWithLocals' first to ensure we have a 'Name' in the
-- type namespace, then call 'dsReify'.

-- See also Note [Using dsReifyTypeNameInfo when promoting instances]
-- in Data.Singletons.Promote.
dsReifyTypeNameInfo :: DsMonad q => Name -> q (Maybe DInfo)
dsReifyTypeNameInfo ty_name = do
  mb_name <- lookupTypeNameWithLocals (nameBase ty_name)
  case mb_name of
    Just n  -> dsReify n
    Nothing -> pure Nothing

-- lift concatMap into a monad
-- could this be more efficient?
concatMapM :: (Monad monad, Monoid monoid, Traversable t)
           => (a -> monad monoid) -> t a -> monad monoid
concatMapM fn list = do
  bss <- mapM fn list
  return $ fold bss

-- make a one-element list
listify :: a -> [a]
listify = (:[])

fstOf3 :: (a,b,c) -> a
fstOf3 (a,_,_) = a

liftFst :: (a -> b) -> (a, c) -> (b, c)
liftFst f (a, c) = (f a, c)

liftSnd :: (a -> b) -> (c, a) -> (c, b)
liftSnd f (c, a) = (c, f a)

snocView :: [a] -> ([a], a)
snocView [] = error "snocView nil"
snocView [x] = ([], x)
snocView (x : xs) = liftFst (x:) (snocView xs)

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = go [] []
  where go bs cs []     = (reverse bs, reverse cs)
        go bs cs (a:as) =
          case f a of
            Left b  -> go (b:bs) cs as
            Right c -> go bs (c:cs) as

partitionWithM :: Monad m => (a -> m (Either b c)) -> [a] -> m ([b], [c])
partitionWithM f = go [] []
  where go bs cs []     = return (reverse bs, reverse cs)
        go bs cs (a:as) = do
          fa <- f a
          case fa of
            Left b  -> go (b:bs) cs as
            Right c -> go bs (c:cs) as

partitionLetDecs :: [DDec] -> ([DLetDec], [DDec])
partitionLetDecs = partitionWith (\case DLetDec ld -> Left ld
                                        dec        -> Right dec)

{-# INLINEABLE zipWith3M #-}
zipWith3M :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWith3M f (a:as) (b:bs) = (:) <$> f a b <*> zipWith3M f as bs
zipWith3M _ _ _ = return []

mapAndUnzip3M :: Monad m => (a -> m (b,c,d)) -> [a] -> m ([b],[c],[d])
mapAndUnzip3M _ []     = return ([],[],[])
mapAndUnzip3M f (x:xs) = do
    (r1,  r2,  r3)  <- f x
    (rs1, rs2, rs3) <- mapAndUnzip3M f xs
    return (r1:rs1, r2:rs2, r3:rs3)

-- is it a letter or underscore?
isHsLetter :: Char -> Bool
isHsLetter c = isLetter c || c == '_'
