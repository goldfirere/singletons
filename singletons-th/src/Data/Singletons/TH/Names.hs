{- Data/Singletons/TH/Names.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

Defining names and manipulations on names for use in promotion and singling.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Singletons.TH.Names where

import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.ShowSing
import Data.Singletons.TH.SuppressUnusedWarnings
import Data.Singletons.TH.Util
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import GHC.TypeLits ( Nat, Symbol )
import GHC.Exts ( Constraint )
import GHC.Show ( showCommaSpace, showSpace )
import Data.String (fromString)
import Data.Type.Equality ( TestEquality(..) )
import Data.Type.Coercion ( TestCoercion(..) )
import Control.Applicative

{-
Note [Wired-in Names]
~~~~~~~~~~~~~~~~~~~~~
The list of Names below contains everything that the Template Haskell machinery
needs to have special knowledge of. These names can be broadly categorized into
two groups:

1. Names of basic singleton definitions (Sing, SingKind, etc.). These are
   spliced directly into TH-generated code.
2. Names of definitions from the Prelude. These are not spliced into
   TH-generated code, but are instead used as the namesakes for promoted and
   singled definitions. For example, the TH machinery must be aware of the Name
   `fromInteger` so that it can promote and single the expression `42` to
   `FromInteger 42` and `sFromInteger (sing @42)`, respectively.

Note that we deliberately do not wire in promoted or singled Names, such as
FromInteger or sFromInteger, for two reasons:

a. We want all promoted and singled names to go through the naming options in
   D.S.TH.Options. Splicing the name FromInteger directly into TH-generated
   code, for instance, would prevent users from overriding the default options
   in order to promote `fromInteger` to something else (e.g.,
   MyCustomFromInteger).
b. Wired in names live in particular modules, so if we were to wire in the name
   FromInteger, it would come from GHC.Num.Singletons. This would effectively
   prevent anyone from defining their own version of FromInteger and
   piggybacking on top of the TH machinery to generate it, however. As a
   result, we generate the name FromInteger completely unqualified so that
   it picks up whichever version of FromInteger is in scope.
-}

boolName, andName, compareName, minBoundName,
  maxBoundName, repName,
  nilName, consName, listName, tyFunArrowName,
  applyName, applyTyConName, applyTyConAux1Name,
  natName, symbolName, stringName,
  eqName, ordName, boundedName, orderingName,
  singFamilyName, singIName, singMethName, demoteName, withSingIName,
  singKindClassName, someSingTypeName, someSingDataName,
  sDecideClassName, sDecideMethName,
  testEqualityClassName, testEqualityMethName, decideEqualityName,
  testCoercionClassName, testCoercionMethName, decideCoercionName,
  provedName, disprovedName, reflName, toSingName, fromSingName,
  equalityName, applySingName, suppressClassName, suppressMethodName,
  thenCmpName, sameKindName, fromIntegerName, negateName,
  errorName, foldlName, cmpEQName, cmpLTName, cmpGTName,
  toEnumName, fromEnumName, enumName,
  equalsName, constraintName,
  showName, showSName, showCharName, showCommaSpaceName, showParenName, showsPrecName,
  showSpaceName, showStringName, showSingName, showSing'Name,
  composeName, gtName, fromStringName,
  foldableName, foldMapName, memptyName, mappendName, foldrName,
  functorName, fmapName, replaceName,
  traversableName, traverseName, pureName, apName, liftA2Name :: Name
boolName = ''Bool
andName = '(&&)
compareName = 'compare
minBoundName = 'minBound
maxBoundName = 'maxBound
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
someSingTypeName = ''SomeSing
someSingDataName = 'SomeSing
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
thenCmpName = 'thenCmp
sameKindName = ''SameKind
fromIntegerName = 'fromInteger
negateName = 'negate
errorName = 'error
foldlName = 'foldl
cmpEQName = 'EQ
cmpLTName = 'LT
cmpGTName = 'GT
toEnumName = 'toEnum
fromEnumName = 'fromEnum
enumName = ''Enum
equalsName = '(==)
constraintName = ''Constraint
showName = ''Show
showSName = ''ShowS
showCharName = 'showChar
showParenName = 'showParen
showSpaceName = 'showSpace
showsPrecName = 'showsPrec
showStringName = 'showString
showSingName = ''ShowSing
showSing'Name = ''ShowSing'
composeName = '(.)
gtName = '(>)
showCommaSpaceName = 'showCommaSpace
fromStringName = 'fromString
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

mkTyName :: Quasi q => Name -> q Name
mkTyName tmName = do
  let nameStr  = nameBase tmName
      symbolic = not (isHsLetter (head nameStr))
  qNewName (if symbolic then "ty" else nameStr)

mkTyConName :: Int -> Name
mkTyConName i = mkName $ "TyCon" ++ show i

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
