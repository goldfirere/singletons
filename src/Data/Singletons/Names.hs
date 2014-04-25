{- Data/Singletons/Names.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Defining names and maniuplations on names for use in promotion and singling.
-}

{-# LANGUAGE CPP, TemplateHaskell #-}

module Data.Singletons.Names where

import Data.Singletons
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.Types
import Data.Singletons.Decide
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import GHC.TypeLits ( Symbol )
import GHC.Exts ( Any )
import Data.Typeable ( TypeRep )
import Data.Singletons.Util

anyTypeName, boolName, andName, tyEqName, repName,
  nilName, consName, listName, tyFunName,
  applyName, symbolName, undefinedName, typeRepName, stringName,
  eqName, ordName, singFamilyName, singIName, singMethName, demoteRepName,
  singKindClassName, sEqClassName, sEqMethName, sconsName, snilName,
  sIfName, kProxyDataName, kProxyTypeName, proxyTypeName, proxyDataName,
  someSingTypeName, someSingDataName,
  sListName, sDecideClassName, sDecideMethName,
  provedName, disprovedName, reflName, toSingName, fromSingName,
  equalityName, applySingName, suppressClassName, suppressMethodName :: Name
anyTypeName = ''Any
boolName = ''Bool
andName = '(&&)
#if __GLASGOW_HASKELL__ >= 707
tyEqName = ''(==)
#else
tyEqName = ''(:==)
#endif
repName = mkName "Rep"
nilName = '[]
consName = '(:)
listName = ''[]
tyFunName = ''TyFun
applyName = ''Apply
symbolName = ''Symbol
undefinedName = 'undefined
typeRepName = ''TypeRep
stringName = ''String
eqName = ''Eq
ordName = ''Ord
singFamilyName = ''Sing
singIName = ''SingI
singMethName = 'sing
toSingName = 'toSing
fromSingName = 'fromSing
demoteRepName = ''DemoteRep
singKindClassName = ''SingKind
sEqClassName = mkName "SEq"
sEqMethName = mkName "%:=="
sIfName = mkName "sIf"
sconsName = mkName "SCons"
snilName = mkName "SNil"
kProxyDataName = 'KProxy
kProxyTypeName = ''KProxy
someSingTypeName = ''SomeSing
someSingDataName = 'SomeSing
proxyTypeName = ''Proxy
proxyDataName = 'Proxy
sListName = mkName "SList"
sDecideClassName = ''SDecide
sDecideMethName = '(%~)
provedName = 'Proved
disprovedName = 'Disproved
reflName = 'Refl
equalityName = ''(~)
applySingName = 'applySing
suppressClassName = ''SuppressUnusedWarnings
suppressMethodName = 'suppressUnusedWarnings

mkTupleName :: Int -> Name
mkTupleName n = mkName $ "STuple" ++ (show n)

-- used when a value name appears in a pattern context
-- works only for proper variables (lower-case names)
promoteValNameLhs :: Name -> Name
promoteValNameLhs = upcase

-- like promoteValNameLhs, but adds a prefix to the promoted name
promoteValNameLhsPrefix :: String -> Name -> Name
promoteValNameLhsPrefix prefix = mkName . (prefix ++) . toUpcaseStr

-- used when a value name appears in an expression context
-- works for both variables and datacons
promoteValRhs :: Name -> DType
promoteValRhs name
  | name == nilName
  = DConT nilName   -- workaround for #21

  | otherwise
  = DConT $ promoteTySym name 0

-- generates type-level symbol for a given name. Int parameter represents
-- saturation: 0 - no parameters passed to the symbol, 1 - one parameter
-- passed to the symbol, and so on. Works on both promoted and unpromoted
-- names.
promoteTySym :: Name -> Int -> Name
promoteTySym name sat
    | name == undefinedName
    = anyTypeName

    | Just degree <- tupleNameDegree_maybe name
    = mkName $ "Tuple" ++ show degree ++ "Sym" ++ (show sat)

       -- treat unboxed tuples like tuples
    | Just degree <- unboxedTupleNameDegree_maybe name
    = mkName $ "Tuple" ++ show degree ++ "Sym" ++ (show sat)

    | otherwise
    = let capped = toUpcaseStr name in
      if head capped == ':'
      then mkName (capped ++ (replicate (sat + 1) '$'))
      else mkName (capped ++ "Sym" ++ (show sat))

-- produce the silly type class used to store the type variables for
-- a class
classTvsName :: Name -> Name
classTvsName = suffixName "TyVars" "^^^"

mkTyName :: Quasi q => Name -> q Name
mkTyName tmName = do
  let nameStr  = nameBase tmName
      symbolic = not (isHsLetter (head nameStr))
  qNewName (if symbolic then "ty" else nameStr)

falseTySym :: DType
falseTySym = promoteValRhs falseName

trueTySym :: DType
trueTySym = promoteValRhs trueName

boolKi :: DKind
boolKi = DConK boolName []

andTySym :: DType
andTySym = promoteValRhs andName

apply :: DType -> DType -> DType
apply t1 t2 = DAppT (DAppT (DConT applyName) t1) t2

-- make a Name with an unknown kind into a DTyVarBndr.
-- Uses a fresh kind variable for GHC 7.6.3 and PlainTV for 7.8+
-- because 7.8+ has kind inference
inferKindTV :: Quasi q => Name -> q DTyVarBndr
inferKindTV n = do
#if __GLASGOW_HASKELL__ < 707
  ki <- fmap DVarK $ qNewName "k"
  return $ DKindedTV n _ki
#else
  return $ DPlainTV n
#endif

inferMaybeKindTV :: Quasi q => Name -> Maybe DKind -> q DTyVarBndr
inferMaybeKindTV n Nothing =
#if __GLASGOW_HASKELL__ < 707
  do k <- qNewName "k"
     return $ DKindedTV n (DVarK k)
#else
  return $ DPlainTV n
#endif
inferMaybeKindTV n (Just k) = return $ DKindedTV n k

-- similar to above, this is for annotating the result kind of
-- a closed type family. Makes it polymorphic in 7.6.3, inferred
-- in 7.8
unknownResult :: DKind -> Maybe DKind
#if __GLASGOW_HASKELL__ < 707
unknownResult = Just
#else
unknownResult = const Nothing
#endif

-- Singletons

singDataConName :: Name -> Name
singDataConName nm
  | nm == nilName                                  = snilName
  | nm == consName                                 = sconsName
  | Just degree <- tupleNameDegree_maybe nm        = mkTupleName degree
  | Just degree <- unboxedTupleNameDegree_maybe nm = mkTupleName degree
  | otherwise                                      = prefixUCName "S" ":%" nm

singTyConName :: Name -> Name
singTyConName name
  | name == listName                                 = sListName
  | Just degree <- tupleNameDegree_maybe name        = mkTupleName degree
  | Just degree <- unboxedTupleNameDegree_maybe name = mkTupleName degree
  | otherwise                                        = prefixUCName "S" ":%" name

singClassName :: Name -> Name
singClassName = singTyConName

singValName :: Name -> Name
singValName n
  | n == undefinedName       = undefinedName
     -- avoid unused variable warnings
  | head (nameBase n) == '_' = (prefixLCName "_s" "%") $ n
  | otherwise                = (prefixLCName "s" "%") $ upcase n

kindParam :: DKind -> DType
kindParam k = DSigT (DConT kProxyDataName) (DConK kProxyTypeName [k])

proxyFor :: DType -> DExp
proxyFor ty = DSigE (DConE proxyDataName) (DAppT (DConT proxyTypeName) ty)

singFamily :: DType
singFamily = DConT singFamilyName

singKindConstraint :: DKind -> DPred
singKindConstraint k = DAppPr (DConPr singKindClassName) (kindParam k)

demote :: DType
demote = DConT demoteRepName
