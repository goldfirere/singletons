{- Data/Singletons/Names.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Defining names and manipulations on names for use in promotion and singling.
-}

{-# LANGUAGE TemplateHaskell, CPP #-}

module Data.Singletons.Names where

import Data.Singletons
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.Decide
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import GHC.TypeLits ( Nat, Symbol )
import GHC.Exts ( Any )
import Data.Typeable ( TypeRep )
import Data.Singletons.Util
import Data.Proxy ( Proxy(..) )
import Control.Monad

anyTypeName, boolName, andName, tyEqName, compareName, minBoundName,
  maxBoundName, repName,
  nilName, consName, listName, tyFunName,
  applyName, natName, symbolName, undefinedName, typeRepName, stringName,
  eqName, ordName, boundedName, orderingName,
  singFamilyName, singIName, singMethName, demoteRepName,
  singKindClassName, sEqClassName, sEqMethName, sconsName, snilName,
  sIfName, kProxyDataName, kProxyTypeName, proxyTypeName, proxyDataName,
  someSingTypeName, someSingDataName,
  sListName, sDecideClassName, sDecideMethName,
  provedName, disprovedName, reflName, toSingName, fromSingName,
  equalityName, applySingName, suppressClassName, suppressMethodName,
  thenCmpName,
  kindOfName, tyFromIntegerName, tyNegateName, sFromIntegerName,
  sNegateName, errorName, foldlName, cmpEQName, cmpLTName, cmpGTName,
  singletonsToEnumName, singletonsFromEnumName, enumName, singletonsEnumName,
  equalsName :: Name
anyTypeName = ''Any
boolName = ''Bool
andName = '(&&)
compareName = 'compare
minBoundName = 'minBound
maxBoundName = 'maxBound
tyEqName = mk_name_tc "Data.Singletons.Prelude.Eq" ":=="
repName = mkName "Rep"   -- this is actually defined in client code!
nilName = '[]
consName = '(:)
listName = ''[]
tyFunName = ''TyFun
applyName = ''Apply
symbolName = ''Symbol
natName = ''Nat
undefinedName = 'undefined
typeRepName = ''TypeRep
stringName = ''String
eqName = ''Eq
ordName = ''Ord
boundedName = ''Bounded
orderingName = ''Ordering
singFamilyName = ''Sing
singIName = ''SingI
singMethName = 'sing
toSingName = 'toSing
fromSingName = 'fromSing
demoteRepName = ''DemoteRep
singKindClassName = ''SingKind
sEqClassName = mk_name_tc "Data.Singletons.Prelude.Eq" "SEq"
sEqMethName = mk_name_v "Data.Singletons.Prelude.Eq" "%:=="
sIfName = mk_name_v "Data.Singletons.Prelude.Bool" "sIf"
sconsName = mk_name_d "Data.Singletons.Prelude.Instances" "SCons"
snilName = mk_name_d "Data.Singletons.Prelude.Instances" "SNil"
kProxyDataName = 'KProxy
kProxyTypeName = ''KProxy
someSingTypeName = ''SomeSing
someSingDataName = 'SomeSing
proxyTypeName = ''Proxy
proxyDataName = 'Proxy
sListName = mk_name_tc "Data.Singletons.Prelude.Instances" "SList"
sDecideClassName = ''SDecide
sDecideMethName = '(%~)
provedName = 'Proved
disprovedName = 'Disproved
reflName = 'Refl
equalityName = ''(~)
applySingName = 'applySing
suppressClassName = ''SuppressUnusedWarnings
suppressMethodName = 'suppressUnusedWarnings
thenCmpName = mk_name_v "Data.Singletons.Prelude.Ord" "thenCmp"
kindOfName = ''KindOf
tyFromIntegerName = mk_name_tc "Data.Singletons.Prelude.Num" "FromInteger"
tyNegateName = mk_name_tc "Data.Singletons.Prelude.Num" "Negate"
sFromIntegerName = mk_name_v "Data.Singletons.Prelude.Num" "sFromInteger"
sNegateName = mk_name_v "Data.Singletons.Prelude.Num" "sNegate"
errorName = 'error
foldlName = 'foldl
cmpEQName = 'EQ
cmpLTName = 'LT
cmpGTName = 'GT
singletonsToEnumName = mk_name_v "Data.Singletons.Prelude.Enum" "toEnum"
singletonsFromEnumName = mk_name_v "Data.Singletons.Prelude.Enum" "fromEnum"
enumName = ''Enum
singletonsEnumName = mk_name_tc "Data.Singletons.Prelude.Enum" "Enum"
equalsName = '(==)

singPkg :: String
singPkg = $( (LitE . StringL . loc_package) `liftM` location )

mk_name_tc :: String -> String -> Name
mk_name_tc = mkNameG_tc singPkg

mk_name_d :: String -> String -> Name
mk_name_d = mkNameG_d singPkg

mk_name_v :: String -> String -> Name
mk_name_v = mkNameG_v singPkg

mkTupleTypeName :: Int -> Name
mkTupleTypeName n = mk_name_tc "Data.Singletons.Prelude.Instances" $
                    "STuple" ++ (show n)

mkTupleDataName :: Int -> Name
mkTupleDataName n = mk_name_d "Data.Singletons.Prelude.Instances" $
                    "STuple" ++ (show n)

-- used when a value name appears in a pattern context
-- works only for proper variables (lower-case names)
promoteValNameLhs :: Name -> Name
promoteValNameLhs = upcase

-- like promoteValNameLhs, but adds a prefix to the promoted name
promoteValNameLhsPrefix :: (String, String) -> Name -> Name
promoteValNameLhsPrefix pres n = mkName $ toUpcaseStr pres n

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

    | name == nilName
    = mkName $ "NilSym" ++ (show sat)

       -- treat unboxed tuples like tuples
    | Just degree <- tupleNameDegree_maybe name `mplus`
                     unboxedTupleNameDegree_maybe name
    = mk_name_tc "Data.Singletons.Prelude.Instances" $
                 "Tuple" ++ show degree ++ "Sym" ++ (show sat)

    | otherwise
    = let capped = toUpcaseStr noPrefix name in
      if isHsLetter (head capped)
      then mkName (capped ++ "Sym" ++ (show sat))
      else mkName (capped ++ (replicate (sat + 1) '$'))

promoteClassName :: Name -> Name
promoteClassName = prefixUCName "P" "#"

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
#if MIN_VERSION_th_desugar(1,6,0)
boolKi = DConT boolName
#else
boolKi = DConK boolName []
#endif

andTySym :: DType
andTySym = promoteValRhs andName

-- Singletons

singDataConName :: Name -> Name
singDataConName nm
  | nm == nilName                                  = snilName
  | nm == consName                                 = sconsName
  | Just degree <- tupleNameDegree_maybe nm        = mkTupleDataName degree
  | Just degree <- unboxedTupleNameDegree_maybe nm = mkTupleDataName degree
  | otherwise                                      = prefixUCName "S" ":%" nm

singTyConName :: Name -> Name
singTyConName name
  | name == listName                                 = sListName
  | Just degree <- tupleNameDegree_maybe name        = mkTupleTypeName degree
  | Just degree <- unboxedTupleNameDegree_maybe name = mkTupleTypeName degree
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
#if MIN_VERSION_th_desugar(1,6,0)
kindParam k = DSigT (DConT kProxyDataName) (DAppT (DConT kProxyTypeName) k)
#else
kindParam k = DSigT (DConT kProxyDataName) (DConK kProxyTypeName [k])
#endif

proxyFor :: DType -> DExp
proxyFor ty = DSigE (DConE proxyDataName) (DAppT (DConT proxyTypeName) ty)

singFamily :: DType
singFamily = DConT singFamilyName

singKindConstraint :: DKind -> DPred
singKindConstraint k = DAppPr (DConPr singKindClassName) (kindParam k)

demote :: DType
demote = DConT demoteRepName

apply :: DType -> DType -> DType
apply t1 t2 = DAppT (DAppT (DConT applyName) t1) t2

mkListE :: [DExp] -> DExp
mkListE =
  foldr (\h t -> DConE consName `DAppE` h `DAppE` t) (DConE nilName)

-- apply a type to a list of types using Apply type family
-- This is defined here, not in Utils, to avoid cyclic dependencies
foldApply :: DType -> [DType] -> DType
foldApply = foldl apply

-- make and equality predicate
mkEqPred :: DType -> DType -> DPred
mkEqPred ty1 ty2 = foldl DAppPr (DConPr equalityName) [ty1, ty2]

-- create a bunch of kproxy vars, and constrain them all to be 'KProxy
mkKProxies :: Quasi q
           => [Name]   -- for the kinds of the kproxies
           -> q ([DTyVarBndr], DCxt)
mkKProxies ns = do
  kproxies <- mapM (const $ qNewName "kproxy") ns
#if MIN_VERSION_th_desugar(1,6,0)
  return ( zipWith (\kp kv -> DKindedTV kp (DAppT (DConT kProxyTypeName) (DVarT kv)))
#else
  return ( zipWith (\kp kv -> DKindedTV kp (DConK kProxyTypeName [DVarK kv]))
#endif
                   kproxies ns
         , map (\kp -> mkEqPred (DVarT kp) (DConT kProxyDataName)) kproxies )
