{- Data/Singletons/Names.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

Defining names and manipulations on names for use in promotion and singling.
-}

{-# LANGUAGE TemplateHaskell #-}

module Data.Singletons.Names where

import Data.Singletons.Internal
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.Decide
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import GHC.TypeLits ( Nat, Symbol )
import GHC.Exts ( Constraint )
import GHC.Show ( showCommaSpace, showSpace )
import Data.Type.Equality ( TestEquality(..) )
import Data.Type.Coercion ( TestCoercion(..) )
import Data.Typeable ( TypeRep )
import Data.Singletons.Util
import Control.Applicative
import Control.Monad

boolName, andName, tyEqName, compareName, minBoundName,
  maxBoundName, repName,
  nilName, consName, listName, tyFunArrowName,
  applyName, applyTyConName, applyTyConAux1Name,
  natName, symbolName, typeRepName, stringName,
  eqName, ordName, boundedName, orderingName,
  singFamilyName, singIName, singMethName, demoteName, withSingIName,
  singKindClassName, sEqClassName, sEqMethName, sconsName, snilName, strueName,
  sIfName,
  someSingTypeName, someSingDataName,
  sListName, sDecideClassName, sDecideMethName,
  testEqualityClassName, testEqualityMethName, decideEqualityName,
  testCoercionClassName, testCoercionMethName, decideCoercionName,
  provedName, disprovedName, reflName, toSingName, fromSingName,
  equalityName, applySingName, suppressClassName, suppressMethodName,
  thenCmpName,
  sameKindName, tyFromIntegerName, tyNegateName, sFromIntegerName,
  sNegateName, errorName, foldlName, cmpEQName, cmpLTName, cmpGTName,
  singletonsToEnumName, singletonsFromEnumName, enumName, singletonsEnumName,
  equalsName, constraintName,
  showName, showSName, showCharName, showCommaSpaceName, showParenName, showsPrecName,
  showSpaceName, showStringName, showSingName, showSing'Name,
  composeName, gtName, tyFromStringName, sFromStringName,
  foldableName, foldMapName, memptyName, mappendName, foldrName,
  functorName, fmapName, replaceName,
  traversableName, traverseName, pureName, apName, liftA2Name :: Name
boolName = ''Bool
andName = '(&&)
compareName = 'compare
minBoundName = 'minBound
maxBoundName = 'maxBound
tyEqName = mk_name_tc "Data.Singletons.Prelude.Eq" "=="
repName = mkName "Rep"   -- this is actually defined in client code!
nilName = '[]
consName = '(:)
listName = ''[]
tyFunArrowName = ''(~>)
applyName = ''Apply
applyTyConName = ''ApplyTyCon
applyTyConAux1Name = ''ApplyTyConAux1
symbolName = ''Symbol
natName = ''Nat
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
demoteName = ''Demote
withSingIName = 'withSingI
singKindClassName = ''SingKind
sEqClassName = mk_name_tc "Data.Singletons.Prelude.Eq" "SEq"
sEqMethName = mk_name_v "Data.Singletons.Prelude.Eq" "%=="
sIfName = mk_name_v "Data.Singletons.Prelude.Bool" "sIf"
sconsName = mk_name_d "Data.Singletons.Prelude.Instances" "SCons"
snilName = mk_name_d "Data.Singletons.Prelude.Instances" "SNil"
strueName = mk_name_d "Data.Singletons.Prelude.Instances" "STrue"
someSingTypeName = ''SomeSing
someSingDataName = 'SomeSing
sListName = mk_name_tc "Data.Singletons.Prelude.Instances" "SList"
sDecideClassName = ''SDecide
sDecideMethName = '(%~)
testEqualityClassName = ''TestEquality
testEqualityMethName = 'testEquality
decideEqualityName = 'decideEquality
testCoercionClassName = ''TestCoercion
testCoercionMethName = 'testCoercion
decideCoercionName = 'decideCoercion
provedName = 'Proved
disprovedName = 'Disproved
reflName = 'Refl
equalityName = ''(~)
applySingName = 'applySing
suppressClassName = ''SuppressUnusedWarnings
suppressMethodName = 'suppressUnusedWarnings
thenCmpName = mk_name_v "Data.Singletons.Prelude.Ord" "thenCmp"
sameKindName = ''SameKind
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
constraintName = ''Constraint
showName = ''Show
showSName = ''ShowS
showCharName = 'showChar
showParenName = 'showParen
showSpaceName = 'showSpace
showsPrecName = 'showsPrec
showStringName = 'showString
showSingName = mk_name_tc "Data.Singletons.ShowSing" "ShowSing"
showSing'Name = mk_name_tc "Data.Singletons.ShowSing" "ShowSing'"
composeName = '(.)
gtName = '(>)
showCommaSpaceName = 'showCommaSpace
tyFromStringName = mk_name_tc "Data.Singletons.Prelude.IsString" "FromString"
sFromStringName = mk_name_v "Data.Singletons.Prelude.IsString" "sFromString"
foldableName = ''Foldable
foldMapName = 'foldMap
memptyName = 'mempty
mappendName = 'mappend
foldrName = 'foldr
functorName = ''Functor
fmapName = 'fmap
replaceName = '(<$)
traversableName = ''Traversable
traverseName = 'traverse
pureName = 'pure
apName = '(<*>)
liftA2Name = 'liftA2

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

mkTyName :: Quasi q => Name -> q Name
mkTyName tmName = do
  let nameStr  = nameBase tmName
      symbolic = not (isHsLetter (head nameStr))
  qNewName (if symbolic then "ty" else nameStr)

mkTyConName :: Int -> Name
mkTyConName i = mk_name_tc "Data.Singletons.Internal" $ "TyCon" ++ show i

boolKi :: DKind
boolKi = DConT boolName

singFamily :: DType
singFamily = DConT singFamilyName

singKindConstraint :: DKind -> DPred
singKindConstraint = DAppT (DConT singKindClassName)

demote :: DType
demote = DConT demoteName

apply :: DType -> DType -> DType
apply t1 t2 = DAppT (DAppT (DConT applyName) t1) t2

mkListE :: [DExp] -> DExp
mkListE =
  foldr (\h t -> DConE consName `DAppE` h `DAppE` t) (DConE nilName)

-- apply a type to a list of types using Apply type family
-- This is defined here, not in Utils, to avoid cyclic dependencies
foldApply :: DType -> [DType] -> DType
foldApply = foldl apply

-- make an equality predicate
mkEqPred :: DType -> DType -> DPred
mkEqPred ty1 ty2 = foldType (DConT equalityName) [ty1, ty2]

-- | If a 'String' begins with one or more underscores, return
-- @'Just' (us, rest)@, where @us@ contain all of the underscores at the
-- beginning of the 'String' and @rest@ contains the remainder of the 'String'.
-- Otherwise, return 'Nothing'.
splitUnderscores :: String -> Maybe (String, String)
splitUnderscores s = case span (== '_') s of
                       ([], _) -> Nothing
                       res     -> Just res

-- Walk a DType, applying a function to all occurrences of constructor names.
modifyConNameDType :: (Name -> Name) -> DType -> DType
modifyConNameDType mod_con_name = go
  where
    go :: DType -> DType
    go (DForallT fvf tvbs p) = DForallT fvf tvbs (go p)
    go (DConstrainedT cxt p) = DConstrainedT (map go cxt) (go p)
    go (DAppT     p t)       = DAppT     (go p) t
    go (DAppKindT p k)       = DAppKindT (go p) k
    go (DSigT     p k)       = DSigT     (go p) k
    go p@(DVarT _)           = p
    go (DConT n)             = DConT (mod_con_name n)
    go p@DWildCardT          = p
    go p@(DLitT {})          = p
    go p@DArrowT             = p

{-
Note [Defunctionalization symbol suffixes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before, we used to denote defunctionalization symbols by simply appending dollar
signs at the end (e.g., (+$) and (+$$)). But this can lead to ambiguity when you
have function names that consist of solely $ characters. For instance, if you
tried to promote ($) and ($$) simultaneously, you'd get these promoted types:

$
$$

And these defunctionalization symbols:

$$
$$$

But now there's a name clash between the promoted type for ($) and the
defunctionalization symbol for ($$)! The solution is to use a precede these
defunctionalization dollar signs with another string (we choose @#@).
So now the new defunctionalization symbols would be:

$@#@$
$@#@$$

And there is no conflict.
-}
