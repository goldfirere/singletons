{-# LANGUAGE LambdaCase #-}

{- Data/Singletons/TH/Util.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains helper functions internal to the singletons-th package.
Users of the package should not need to consult this file.
-}

module Data.Singletons.TH.Util where

import Prelude hiding ( exp, foldl, concat, mapM, any, pred )
import Language.Haskell.TH ( pprint )
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import Data.Char
import Control.Monad ( liftM, unless, when )
import Control.Monad.Except ( ExceptT, runExceptT, MonadError(..) )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( MonadReader(..), Reader, ReaderT(..) )
import Control.Monad.Trans ( MonadTrans )
import Control.Monad.Writer ( MonadWriter(..), WriterT(..), execWriterT )
import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Bifunctor (second)
import Data.Foldable
import Data.Functor.Identity
import Data.Traversable
import Data.Generics
import Data.Maybe

import Data.Singletons.TH.Syntax.LocalVar

-- like reportWarning, but generalized to any Quasi
qReportWarning :: Quasi q => String -> q ()
qReportWarning = qReport False

-- like reportError, but generalized to any Quasi
qReportError :: Quasi q => String -> q ()
qReportError = qReport True

-- | Generate a new Unique
qNewUnique :: DsMonad q => q Uniq
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

recSelsOfConFields :: DConFields -> [Name]
recSelsOfConFields DNormalC{}    = []
recSelsOfConFields (DRecC vstys) = map (\(n,_,_) -> n) vstys

-- Extract a data constructor's name and the number of arguments it accepts.
extractNameArgs :: DCon -> (Name, Int)
extractNameArgs (DCon _ _ n fields _) = (n, length (tysOfConFields fields))

-- Extract a data constructor's name.
extractName :: DCon -> Name
extractName (DCon _ _ n _ _) = n

-- Extract the names of a data constructor's record selectors.
extractRecSelNames :: DCon -> [Name]
extractRecSelNames (DCon _ _ _ fields _) = recSelsOfConFields fields

-- | is a valid Haskell infix data constructor (i.e., does it begin with a colon?)
isInfixDataCon :: String -> Bool
isInfixDataCon (':':_) = True
isInfixDataCon _       = False

-- | Is an identifier a legal data constructor name in Haskell? That is, is its
-- first character an uppercase letter (prefix) or a colon (infix)?
isDataConName :: Name -> Bool
isDataConName n = let first = headNameStr (nameBase n) in isUpper first || first == ':'

-- | Is an identifier uppercase?
--
-- Note that this will always return 'False' for infix names, since the concept
-- of upper- and lower-case doesn't make sense for non-alphabetic characters.
-- If you want to check if a name is legal as a data constructor, use the
-- 'isDataConName' function.
isUpcase :: Name -> Bool
isUpcase n = let first = headNameStr (nameBase n) in isUpper first

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
    first = headNameStr str

    upcase_alpha = alpha ++ (toUpper first) : tailNameStr str
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
      first = headNameStr str in
    if isHsLetter first
     then mkName (pre ++ str)
     else mkName (tyPre ++ str)

-- Put a suffix on a name. Takes two suffixes: one for identifiers
-- and one for symbols.
suffixName :: String -> String -> Name -> Name
suffixName ident symb n =
  let str = nameBase n
      first = headNameStr str in
  if isHsLetter first
  then mkName (str ++ ident)
  else mkName (str ++ symb)

-- Return the first character in a Name's string (i.e., nameBase).
-- Precondition: the string is non-empty.
headNameStr :: String -> Char
headNameStr str =
  case str of
    (c:_) -> c
    [] -> error "headNameStr: Expected non-empty string"

-- Drop the first character in a Name's string (i.e., nameBase).
-- Precondition: the string is non-empty.
tailNameStr :: String -> String
tailNameStr str =
  case str of
    (_:cs) -> cs
    [] -> error "tailNameStr: Expected non-empty string"

-- convert a number into both alphanumeric and symoblic forms
uniquePrefixes :: String   -- alphanumeric prefix
               -> String   -- symbolic prefix
               -> Uniq
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
extractTvbKind :: DTyVarBndr flag -> Maybe DKind
extractTvbKind (DPlainTV _ _)    = Nothing
extractTvbKind (DKindedTV _ _ k) = Just k

-- extract the name from a TyVarBndr.
extractTvbName :: DTyVarBndr flag -> Name
extractTvbName (DPlainTV n _)    = n
extractTvbName (DKindedTV n _ _) = n

-- extract the flag from a TyVarBndr.
extractTvbFlag :: DTyVarBndr flag -> flag
extractTvbFlag (DPlainTV _ f)    = f
extractTvbFlag (DKindedTV _ f _) = f

-- Map over the 'Name' of a 'DTyVarBndr'.
mapDTVName :: (Name -> Name) -> DTyVarBndr flag -> DTyVarBndr flag
mapDTVName f (DPlainTV name flag) = DPlainTV (f name) flag
mapDTVName f (DKindedTV name flag kind) = DKindedTV (f name) flag kind

-- Map over the 'DKind' of a 'DTyVarBndr'.
mapDTVKind :: (DKind -> DKind) -> DTyVarBndr flag -> DTyVarBndr flag
mapDTVKind _ tvb@(DPlainTV{}) = tvb
mapDTVKind f (DKindedTV name flag kind) = DKindedTV name flag (f kind)

tvbToType :: DTyVarBndr flag -> DType
tvbToType = DVarT . extractTvbName

-- If a type variable binder lacks an explicit kind, pick a default kind of
-- Type. Otherwise, leave the binder alone.
defaultTvbToTypeKind :: DTyVarBndr flag -> DTyVarBndr flag
defaultTvbToTypeKind (DPlainTV tvname f) = DKindedTV tvname f $ DConT typeKindName
defaultTvbToTypeKind tvb                 = tvb

-- If @Nothing@, return @Type@. If @Just k@, return @k@.
defaultMaybeToTypeKind :: Maybe DKind -> DKind
defaultMaybeToTypeKind (Just k) = k
defaultMaybeToTypeKind Nothing  = DConT typeKindName

inferMaybeKindTV :: Name -> Maybe DKind -> DTyVarBndrUnit
inferMaybeKindTV n Nothing  = DPlainTV n ()
inferMaybeKindTV n (Just k) = DKindedTV n () k

resultSigToMaybeKind :: DFamilyResultSig -> Maybe DKind
resultSigToMaybeKind DNoSig                        = Nothing
resultSigToMaybeKind (DKindSig k)                  = Just k
resultSigToMaybeKind (DTyVarSig DPlainTV{})        = Nothing
resultSigToMaybeKind (DTyVarSig (DKindedTV _ _ k)) = Just k

maybeKindToResultSig :: Maybe DKind -> DFamilyResultSig
maybeKindToResultSig = maybe DNoSig DKindSig

maybeSigT :: DType -> Maybe DKind -> DType
maybeSigT ty Nothing   = ty
maybeSigT ty (Just ki) = ty `DSigT` ki

-- | Convert a list of 'DTyVarBndrSpec's to a list of 'DTyVarBndrVis'es. Type
-- variable binders with a 'SpecifiedSpec' are converted to 'BndrInvis', and
-- type variable binders with an 'InferredSpec' are dropped entirely.
--
-- As an example, if you have this list of 'DTyVarBndrSpec's:
--
-- @
-- forall a {b} c {d e} f. <...>
-- @
--
-- The corresponding list of 'DTyVarBndrVis'es would be:
--
-- @
-- \@a \@b \@f
-- @
--
-- Note that note of @b@, @d@, or @e@ appear in the list.
--
-- See also 'tvbForAllTyFlagsToBndrVis', which takes a list of @'DTyVarBndr'
-- 'ForAllTyFlag'@ as arguments instead of a list of 'DTyVarBndrSpec's. Note
-- that @'tvbSpecsToBndrVis' . 'tvbForAllTyFlagsToSpecs'@ is /not/ the same
-- thing as 'tvbForAllTyFlagsToBndrVis'. This is because 'tvbSpecsToBndrVis'
-- only produces 'BndrInvis' binders as output, whereas
-- 'tvbForAllTyFlagsToBndrVis' can produce both 'BndrReq' and 'BndrInvis'
-- binders.
tvbSpecsToBndrVis :: [DTyVarBndrSpec] -> [DTyVarBndrVis]
tvbSpecsToBndrVis = mapMaybe (traverse specificityToBndrVis)
  where
    specificityToBndrVis :: Specificity -> Maybe BndrVis
    specificityToBndrVis SpecifiedSpec = Just BndrInvis
    specificityToBndrVis InferredSpec  = Nothing

-- Reconstruct a vanilla function type from its individual type variable
-- binders, constraints, argument types, and result type. (See
-- Note [Vanilla-type validity checking during promotion] in
-- Data.Singletons.TH.Promote.Type for what "vanilla" means.)
ravelVanillaDType :: [DTyVarBndrSpec] -> DCxt -> [DType] -> DType -> DType
ravelVanillaDType tvbs ctxt args res =
  ifNonEmpty tvbs (DForallT . DForallInvis) $
  ifNonEmpty ctxt DConstrainedT $
  go args
  where
    ifNonEmpty :: [a] -> ([a] -> b -> b) -> b -> b
    ifNonEmpty [] _ z = z
    ifNonEmpty l  f z = f l z

    go :: [DType] -> DType
    go []    = res
    go (h:t) = DAppT (DAppT DArrowT h) (go t)

-- Decompose a vanilla function type into its type variables, its context, its
-- argument types, and its result type. (See
-- Note [Vanilla-type validity checking during promotion] in
-- Data.Singletons.TH.Promote.Type for what "vanilla" means.)
-- If a non-vanilla construct is encountered while decomposing the function
-- type, an error is thrown monadically.
--
-- This should be contrasted with the 'unravelDType' function from
-- @th-desugar@, which supports the full gamut of function types. @singletons-th@
-- only supports a subset of these types, which is why this function is used
-- to decompose them instead.
unravelVanillaDType :: forall m. MonadFail m
                    => DType -> m ([DTyVarBndrSpec], DCxt, [DType], DType)
unravelVanillaDType ty =
  case unravelVanillaDType_either ty of
    Left err      -> fail err
    Right payload -> pure payload

-- Ensures that a 'DType' is a vanilla type. (See
-- Note [Vanilla-type validity checking during promotion] in
-- Data.Singletons.TH.Promote.Type for what "vanilla" means.)
--
-- The only monadic thing that this function can do is 'fail', which it does
-- if a non-vanilla construct is encountered.
checkVanillaDType :: forall m. MonadFail m => DType -> m ()
checkVanillaDType ty =
  case unravelVanillaDType_either ty of
    Left err -> fail err
    Right _  -> pure ()

-- The workhorse that powers unravelVanillaDType and checkVanillaDType.
-- Returns @Right payload@ upon success, and @Left error_msg@ upon failure.
unravelVanillaDType_either ::
  DType -> Either String ([DTyVarBndrSpec], DCxt, [DType], DType)
unravelVanillaDType_either ty =
  runIdentity $ flip runReaderT True $ runExceptT $ runUnravelM $ go_ty ty
  where
    go_ty :: DType -> UnravelM ([DTyVarBndrSpec], DCxt, [DType], DType)
    go_ty typ = do
      let (args1, res) = unravelDType typ
      (args2, tvbs) <- take_tvbs  args1
      (args3, ctxt) <- take_ctxt  args2
      anons         <- take_anons args3
      pure (tvbs, ctxt, anons, res)

    -- Process a type in a higher-order position (e.g., the @forall a. a -> a@ in
    -- @(forall a. a -> a) -> b -> b@). This is only done to check for the
    -- presence of higher-rank foralls or constraints, which are not permitted
    -- in vanilla types.
    go_higher_order_ty :: DType -> UnravelM ()
    go_higher_order_ty typ = () <$ local (const False) (go_ty typ)

    take_tvbs :: DFunArgs -> UnravelM (DFunArgs, [DTyVarBndrSpec])
    take_tvbs (DFAForalls (DForallInvis tvbs) args) = do
      rank_1 <- ask
      unless rank_1 $ fail_forall "higher-rank"
      _ <- traverse_ (traverse_ go_higher_order_ty . extractTvbKind) tvbs
      (args', tvbs') <- take_tvbs args
      pure (args', tvbs ++ tvbs')
    take_tvbs (DFAForalls DForallVis{} _) = fail_vdq
    take_tvbs args = pure (args, [])

    take_ctxt :: DFunArgs -> UnravelM (DFunArgs, DCxt)
    take_ctxt (DFACxt ctxt args) = do
      rank_1 <- ask
      unless rank_1 $ fail_ctxt "higher-rank"
      traverse_ go_higher_order_ty ctxt
      (args', ctxt') <- take_ctxt args
      pure (args', ctxt ++ ctxt')
    take_ctxt (DFAForalls tele _) =
      case tele of
        DForallInvis{} -> fail_forall "nested"
        DForallVis{}   -> fail_vdq
    take_ctxt args = pure (args, [])

    take_anons :: DFunArgs -> UnravelM [DType]
    take_anons (DFAAnon anon args) = do
      go_higher_order_ty anon
      anons <- take_anons args
      pure (anon:anons)
    take_anons (DFAForalls tele _) =
      case tele of
        DForallInvis{} -> fail_forall "nested"
        DForallVis{}   -> fail_vdq
    take_anons (DFACxt _ _) = fail_ctxt "nested"
    take_anons DFANil = pure []

    failWith :: MonadError String m => String -> m a
    failWith thing = throwError $ unlines
      [ "`singletons-th` does not support " ++ thing
      , "In the type: " ++ pprint (sweeten ty)
      ]

    fail_forall :: MonadError String m => String -> m a
    fail_forall sort = failWith $ sort ++ " `forall`s"

    fail_vdq :: MonadError String m => m a
    fail_vdq = failWith "visible dependent quantification"

    fail_ctxt :: MonadError String m => String -> m a
    fail_ctxt sort = failWith $ sort ++ " contexts"

-- The monad that powers the internals of unravelVanillaDType_either.
--
-- * ExceptT String: records the error message upon failure.
--
-- * Reader Bool: True if we are in a rank-1 position in a type, False otherwise
newtype UnravelM a = UnravelM { runUnravelM :: ExceptT String (Reader Bool) a }
  deriving (Functor, Applicative, Monad, MonadError String, MonadReader Bool)

-- count the number of arguments in a type
countArgs :: DType -> Int
countArgs ty = length $ filterDVisFunArgs args
  where (args, _) = unravelDType ty

-- Collect the invisible type variable binders from a sequence of DFunArgs.
filterInvisTvbArgs :: DFunArgs -> [DTyVarBndrSpec]
filterInvisTvbArgs DFANil           = []
filterInvisTvbArgs (DFACxt  _ args) = filterInvisTvbArgs args
filterInvisTvbArgs (DFAAnon _ args) = filterInvisTvbArgs args
filterInvisTvbArgs (DFAForalls tele args) =
  let res = filterInvisTvbArgs args in
  case tele of
    DForallVis   _     -> res
    DForallInvis tvbs' -> tvbs' ++ res

-- Change all unique Names with a NameU or NameL namespace to non-unique Names
-- by performing a syb-based traversal. See Note [Pitfalls of NameU/NameL] for
-- why this is useful.
noExactTyVars :: Data a => a -> a
noExactTyVars = everywhere go
  where
    go :: Data a => a -> a
    go = mkT (fix_tvb @Specificity)
      `extT` fix_tvb @()
      `extT` fix_tvb @BndrVis
      `extT` fix_ty
      `extT` fix_inj_ann
      `extT` fix_local_var

    fix_tvb :: Typeable flag => DTyVarBndr flag -> DTyVarBndr flag
    fix_tvb (DPlainTV n f)    = DPlainTV (noExactName n) f
    fix_tvb (DKindedTV n f k) = DKindedTV (noExactName n) f k

    fix_ty (DVarT n)           = DVarT (noExactName n)
    fix_ty ty                  = ty

    fix_inj_ann (InjectivityAnn lhs rhs)
      = InjectivityAnn (noExactName lhs) (map noExactName rhs)

    fix_local_var :: LocalVar -> LocalVar
    fix_local_var (LocalVar { lvName = n, lvKind =  mbKind })
      = LocalVar { lvName = noExactName n, lvKind = mbKind }

-- Changes a unique Name with a NameU or NameL namespace to a non-unique Name.
-- See Note [Pitfalls of NameU/NameL] for why this is useful.
noExactName :: Name -> Name
noExactName n@(Name (OccName occ) ns) =
  case ns of
    NameU unique -> mk_name unique
    NameL unique -> mk_name unique
    _            -> n
  where
    mk_name unique = mkName (occ ++ show unique)

{-
Note [Pitfalls of NameU/NameL]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the Names used in singletons-th come from reified or quoted Template
Haskell definitions. Because these definitions have passed through GHC's
renamer, they have unique Names with unique a NameU/NameL namespace. For the
sake of convenience, we often reuse these Names in the definitions that we
generate. For example, if singletons-th is given a declaration
`f :: forall a_123. a_123 -> a_123`, it will produce a standalone kind signature
`type F :: forall a_123. a_123 -> a_123`, reusing the unique Name `a_123`.

While reusing unique Names is convenient, it does have a downside. In
particular, GHC can sometimes get confused when the same unique Name is reused
in distinct type variable scopes. In the best case, this can lead to confusing
type errors, but in the worst case, it can cause GHC to panic, as seen in the
following issues (all of which were first observed in singletons-th):

* https://gitlab.haskell.org/ghc/ghc/-/issues/11812
* https://gitlab.haskell.org/ghc/ghc/-/issues/17537
* https://gitlab.haskell.org/ghc/ghc/-/issues/19743

This is pretty terrible. Arguably, we are abusing Template Haskell here, since
GHC likely assumes the invariant that each unique Name only has a single
binding site. On the other hand, rearchitecting singletons-th to uphold this
invariant would require a substantial amount of work.

A far easier solution is to identify any problematic areas where unique Names
are reused and work around the issue by changing unique Names to non-unique
Names. The issues above all have a common theme: they arise when unique Names
are reused in the type variable binders of a data type or type family
declaration. For instance, when promoting a function like this:

  f :: forall a_123. a_123 -> a_123
  f x_456 = g
    where
      g = x_456

We must promote `f` and `g` to something like this:

    type F :: forall a_123. a_123 -> a_123
    type family F (arg :: a_123) :: a_123 where
      F x_456 = G x_456

    type family LetG x_456 where
      LetG x_456 = x_456

This looks sensible enough. But note that we are reusing the same unique Name
`x_456` in three different scopes: once in the equation for `F`, once in the
the equation for `G`, and once more in the type variable binder in
`type family LetG x_456`. The last of these scopes in particular is enough to
confuse GHC in some situations and trigger GHC#11812.

Our workaround is to apply the `noExactName` function to such names, which
converts any Names with NameU/NameL namespaces into non-unique Names with
longer OccNames. For instance, `noExactName x_456` will return a non-unique
Name with the OccName `x456`. We use `noExactName` when generating `LetG` so
that it will instead be:

    type family LetG x456 where
      LetG x_456 = x_456

Here, `x456` is a non-unique Name, and `x_456` is a Unique name. Thankfully,
this is sufficient to work around GHC#11812. There is still some amount of
risk, since we are reusing `x_456` in two different type family equations (one
for `LetG` and one for `F`), but GHC accepts this for now. We prefer to use the
`noExactName` in as few places as possible, as using longer OccNames makes the
Haddocks harder to read, so we will continue to reuse unique Names unless GHC
forces us to behave differently.

In addition to the type family example above, we also make use of `noExactName`
(as well as its cousin, `noExactTyVars`) when generating defunctionalization
symbols, as these also require reusing Unique names in several type family and
data type declarations. See references to this Note in the code for particular
locations where we must apply this workaround.
-}

substKind :: Map Name DKind -> DKind -> DKind
substKind = substType

-- | Non–capture-avoiding substitution. (If you want capture-avoiding
-- substitution, use @substTy@ from "Language.Haskell.TH.Desugar.Subst".
substType :: Map Name DType -> DType -> DType
substType subst ty | Map.null subst = ty
substType subst (DForallT tele inner_ty)
  = DForallT tele' inner_ty'
  where
    (subst', tele') = subst_tele subst tele
    inner_ty'       = substType subst' inner_ty
substType subst (DConstrainedT cxt inner_ty) =
  DConstrainedT (map (substType subst) cxt) (substType subst inner_ty)
substType subst (DAppT ty1 ty2) = substType subst ty1 `DAppT` substType subst ty2
substType subst (DAppKindT ty ki) = substType subst ty `DAppKindT` substType subst ki
substType subst (DSigT ty ki) = substType subst ty `DSigT` substType subst ki
substType subst (DVarT n) =
  case Map.lookup n subst of
    Just ki -> ki
    Nothing -> DVarT n
substType _ ty@(DConT {}) = ty
substType _ ty@(DArrowT)  = ty
substType _ ty@(DLitT {}) = ty
substType _ ty@DWildCardT = ty

subst_tele :: Map Name DKind -> DForallTelescope -> (Map Name DKind, DForallTelescope)
subst_tele s (DForallInvis tvbs) = second DForallInvis $ substTvbs s tvbs
subst_tele s (DForallVis   tvbs) = second DForallVis   $ substTvbs s tvbs

substTvbs :: Map Name DKind -> [DTyVarBndr flag] -> (Map Name DKind, [DTyVarBndr flag])
substTvbs = mapAccumL substTvb

substTvb :: Map Name DKind -> DTyVarBndr flag -> (Map Name DKind, DTyVarBndr flag)
substTvb s tvb@(DPlainTV n _) = (Map.delete n s, tvb)
substTvb s (DKindedTV n f k)  = (Map.delete n s, DKindedTV n f (substKind s k))

substFamilyResultSig :: Map Name DKind -> DFamilyResultSig -> (Map Name DKind, DFamilyResultSig)
substFamilyResultSig s frs@DNoSig      = (s, frs)
substFamilyResultSig s (DKindSig k)    = (s, DKindSig (substKind s k))
substFamilyResultSig s (DTyVarSig tvb) = let (s', tvb') = substTvb s tvb in
                                         (s', DTyVarSig tvb')

dropTvbKind :: DTyVarBndr flag -> DTyVarBndr flag
dropTvbKind tvb@(DPlainTV {}) = tvb
dropTvbKind (DKindedTV n f _) = DPlainTV n f

-- apply a type to a list of types
foldType :: DType -> [DType] -> DType
foldType = foldl DAppT

-- apply a type to a list of type variable binders
foldTypeTvbs :: DType -> [DTyVarBndrVis] -> DType
foldTypeTvbs ty = applyDType ty . map dTyVarBndrVisToDTypeArg

-- Construct a data type's variable binders, possibly using fresh variables
-- from the data type's kind signature. This function is used when constructing
-- a @DataDecl@ to ensure that it has a number of binders equal in length to the
-- number of visible quantifiers (i.e., the number of function arrows plus the
-- number of visible @forall@–bound variables) in the data type's kind.
buildDataDTvbs :: DsMonad q => [DTyVarBndrVis] -> Maybe DKind -> q [DTyVarBndrVis]
buildDataDTvbs tvbs mk = do
  extra_tvbs <- mkExtraDKindBinders $ fromMaybe (DConT typeKindName) mk
  pure $ tvbs ++ extra_tvbs

-- apply an expression to a list of expressions
foldExp :: DExp -> [DExp] -> DExp
foldExp = foldl DAppE

-- choose the first non-empty list
orIfEmpty :: [a] -> [a] -> [a]
orIfEmpty [] x = x
orIfEmpty x  _ = x

-- build a pattern match over several expressions, each with only one pattern
multiCase :: [DExp] -> [DPat] -> DExp -> DExp
multiCase [] [] body = body
multiCase scruts pats body = dCasesE scruts [DClause pats body]

-- a monad transformer for writing a monoid alongside returning a Q
newtype QWithAux m q a = QWA { runQWA :: WriterT m q a }
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadWriter m, MonadReader r
           , MonadFail, MonadIO, Quasi, DsMonad )

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
-- in Data.Singletons.TH.Promote.
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

-- like GHC's
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM _ [] = return []
mapMaybeM f (x:xs) = do
  y <- f x
  ys <- mapMaybeM f xs
  return $ case y of
    Nothing -> ys
    Just z  -> z : ys

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

-- @'matchUpSAKWithDecl' decl_sak decl_bndrs@ produces @'DTyVarBndr'
-- 'ForAllTyFlag'@s for a declaration, using the original declaration's
-- standalone kind signature (@decl_sak@) and its user-written binders
-- (@decl_bndrs@) as a template. For this example:
--
-- @
-- type D :: forall j k. k -> j -> Type
-- data D \@j \@l (a :: l) b = ...
-- @
--
-- We would produce the following @'DTyVarBndr' 'ForAllTyFlag'@s:
--
-- @
-- \@j \@l (a :: l) (b :: j)
-- @
--
-- From here, these @'DTyVarBndr' 'ForAllTyFlag'@s can be converted into other
-- forms of 'DTyVarBndr's:
--
-- * They can be converted to 'DTyVarBndrSpec's using 'tvbForAllTyFlagsToSpecs'.
--   (See, for example, 'singDataSAK' in "Data.Singletons.TH.Single.Data", which
--   does this to construct the invisible @forall@s in the standalone kind
--   signature for a singled @data@ declaration.)
--
-- * They can be converted to 'DTyVarBndrVis'es using 'tvbForAllTyFlagsToVis'.
--
-- Note that:
--
-- * This function has a precondition that the length of @decl_bndrs@ must
--   always be equal to the number of visible quantifiers (i.e., the number of
--   function arrows plus the number of visible @forall@–bound variables) in
--   @decl_sak@.
--
-- * Whenever possible, this function reuses type variable names from the
--   declaration's user-written binders. This is why the @'DTyVarBndr'
--   'ForAllTyFlag'@ use @\@j \@l@ instead of @\@j \@k@, since the @(a :: l)@
--   binder uses @l@ instead of @k@. We could have just as well chose the other
--   way around, but we chose to pick variable names from the user-written
--   binders since they scope over other parts of the declaration. (For example,
--   the user-written binders of a @data@ declaration scope over the type
--   variables mentioned in a @deriving@ clause.) As such, keeping these names
--   avoids having to perform some alpha-renaming.
--
-- This function's implementation was heavily inspired by parts of GHC's
-- kcCheckDeclHeader_sig function:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/1464a2a8de082f66ae250d63ab9d94dbe2ef8620/compiler/GHC/Tc/Gen/HsType.hs#L2524-2643
matchUpSAKWithDecl ::
     forall q.
     MonadFail q
  => DKind
     -- ^ The declaration's standalone kind signature
  -> [DTyVarBndrVis]
     -- ^ The user-written binders in the declaration
  -> q [DTyVarBndr ForAllTyFlag]
matchUpSAKWithDecl decl_sak decl_bndrs = do
  -- (1) First, explicitly quantify any free kind variables in `decl_sak` using
  -- an invisible @forall@. This is done to ensure that precondition (2) in
  -- `matchUpSigWithDecl` is upheld. (See the Haddocks for that function).
  let decl_sak_free_tvbs =
        changeDTVFlags SpecifiedSpec $ toposortTyVarsOf [decl_sak]
      decl_sak' = DForallT (DForallInvis decl_sak_free_tvbs) decl_sak

  -- (2) Next, compute type variable binders using `matchUpSigWithDecl`. Note
  -- that these can be biased towards type variable names mention in `decl_sak`
  -- over names mentioned in `decl_bndrs`, but we will fix that up in the next
  -- step.
  let (decl_sak_args, _) = unravelDType decl_sak'
  sing_sak_tvbs <- matchUpSigWithDecl decl_sak_args decl_bndrs

  -- (3) Finally, swizzle the type variable names so that names in `decl_bndrs`
  -- are preferred over names in `decl_sak`.
  --
  -- This is heavily inspired by similar code in GHC:
  -- https://gitlab.haskell.org/ghc/ghc/-/blob/cec903899234bf9e25ea404477ba846ac1e963bb/compiler/GHC/Tc/Gen/HsType.hs#L2607-2616
  let invis_decl_sak_args = filterInvisTvbArgs decl_sak_args
      invis_decl_sak_arg_nms = map extractTvbName invis_decl_sak_args

      invis_decl_bndrs = toposortKindVarsOfTvbs decl_bndrs
      invis_decl_bndr_nms = map extractTvbName invis_decl_bndrs

      swizzle_env =
        Map.fromList $ zip invis_decl_sak_arg_nms invis_decl_bndr_nms
      (_, swizzled_sing_sak_tvbs) =
        mapAccumL (swizzleTvb swizzle_env) Map.empty sing_sak_tvbs
  pure swizzled_sing_sak_tvbs

-- Match the quantifiers in a type-level declaration's standalone kind signature
-- with the user-written binders in the declaration. This function assumes the
-- following preconditions:
--
-- 1. The number of required binders in the declaration's user-written binders
--    is equal to the number of visible quantifiers (i.e., the number of
--    function arrows plus the number of visible @forall@–bound variables) in
--    the standalone kind signature.
--
-- 2. The number of invisible \@-binders in the declaration's user-written
--    binders is less than or equal to the number of invisible quantifiers
--    (i.e., the number of invisible @forall@–bound variables) in the
--    standalone kind signature.
--
-- The implementation of this function is heavily based on a GHC function of
-- the same name:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/1464a2a8de082f66ae250d63ab9d94dbe2ef8620/compiler/GHC/Tc/Gen/HsType.hs#L2645-2715
matchUpSigWithDecl ::
     forall q.
     MonadFail q
  => DFunArgs
     -- ^ The quantifiers in the declaration's standalone kind signature
  -> [DTyVarBndrVis]
     -- ^ The user-written binders in the declaration
  -> q [DTyVarBndr ForAllTyFlag]
matchUpSigWithDecl = go_fun_args Map.empty
  where
    go_fun_args ::
         DSubst
         -- ^ A substitution from the names of @forall@-bound variables in the
         -- standalone kind signature to corresponding binder names in the
         -- user-written binders. This is because we want to reuse type variable
         -- names from the user-written binders whenever possible. For example:
         --
         -- @
         -- type T :: forall a. forall b -> Maybe (a, b) -> Type
         -- data T @x y z
         -- @
         --
         -- After matching up the @a@ in @forall a.@ with @x@ and
         -- the @b@ in @forall b ->@ with @y@, this substitution will be
         -- extended with @[a :-> x, b :-> y]@. This ensures that we will
         -- produce @Maybe (x, y)@ instead of @Maybe (a, b)@ in
         -- the kind for @z@.
      -> DFunArgs -> [DTyVarBndrVis] -> q [DTyVarBndr ForAllTyFlag]
    go_fun_args _ DFANil [] =
      pure []
    -- This should not happen, per the function's precondition
    go_fun_args _ DFANil decl_bndrs =
      fail $ "matchUpSigWithDecl.go_fun_args: Too many binders: " ++ show decl_bndrs
    -- GHC now disallows kind-level constraints, per this GHC proposal:
    -- https://github.com/ghc-proposals/ghc-proposals/blob/b0687d96ce8007294173b7f628042ac4260cc738/proposals/0547-no-kind-equalities.rst
    go_fun_args _ (DFACxt{}) _ =
      fail "matchUpSigWithDecl.go_fun_args: Unexpected kind-level constraint"
    go_fun_args subst (DFAForalls (DForallInvis tvbs) sig_args) decl_bndrs =
      go_invis_tvbs subst tvbs sig_args decl_bndrs
    go_fun_args subst (DFAForalls (DForallVis tvbs) sig_args) decl_bndrs =
      go_vis_tvbs subst tvbs sig_args decl_bndrs
    go_fun_args subst (DFAAnon anon sig_args) (decl_bndr:decl_bndrs) = do
      let decl_bndr_name = extractTvbName decl_bndr
          mb_decl_bndr_kind = extractTvbKind decl_bndr
          anon' = substType subst anon

          anon'' =
            case mb_decl_bndr_kind of
              Nothing -> anon'
              Just decl_bndr_kind ->
                let mb_match_subst = matchTy NoIgnore decl_bndr_kind anon' in
                maybe decl_bndr_kind (`substType` decl_bndr_kind) mb_match_subst
      sig_args' <- go_fun_args subst sig_args decl_bndrs
      pure $ DKindedTV decl_bndr_name Required anon'' : sig_args'
    -- This should not happen, per precondition (1).
    go_fun_args _ _ [] =
      fail "matchUpSigWithDecl.go_fun_args: Too few binders"

    go_invis_tvbs :: DSubst -> [DTyVarBndrSpec] -> DFunArgs -> [DTyVarBndrVis] -> q [DTyVarBndr ForAllTyFlag]
    go_invis_tvbs subst [] sig_args decl_bndrs =
      go_fun_args subst sig_args decl_bndrs
    -- This should not happen, per precondition (2).
    go_invis_tvbs _ (_:_) _ [] =
      fail $ "matchUpSigWithDecl.go_invis_tvbs: Too few binders"
    go_invis_tvbs subst (invis_tvb:invis_tvbs) sig_args decl_bndrss@(decl_bndr:decl_bndrs) =
      case extractTvbFlag decl_bndr of
        -- If the next decl_bndr is required, then we have a invisible forall in
        -- the kind without a corresponding invisible @-binder, which is
        -- allowed. In this case, we simply apply the substitution and recurse.
        BndrReq -> do
          let (subst', invis_tvb') = substTvb subst invis_tvb
          sig_args' <- go_invis_tvbs subst' invis_tvbs sig_args decl_bndrss
          pure $ fmap Invisible invis_tvb' : sig_args'
        -- If the next decl_bndr is an invisible @-binder, then we must match it
        -- against the invisible forall–bound variable in the kind.
        BndrInvis -> do
          let (subst', sig_tvb) = match_tvbs subst invis_tvb decl_bndr
          sig_args' <- go_invis_tvbs subst' invis_tvbs sig_args decl_bndrs
          pure (fmap Invisible sig_tvb : sig_args')

    go_vis_tvbs :: DSubst -> [DTyVarBndrUnit] -> DFunArgs -> [DTyVarBndrVis] -> q [DTyVarBndr ForAllTyFlag]
    go_vis_tvbs subst [] sig_args decl_bndrs =
      go_fun_args subst sig_args decl_bndrs
    -- This should not happen, per precondition (1).
    go_vis_tvbs _ (_:_) _ [] =
      fail $ "matchUpSigWithDecl.go_vis_tvbs: Too few binders"
    go_vis_tvbs subst (vis_tvb:vis_tvbs) sig_args (decl_bndr:decl_bndrs) = do
      case extractTvbFlag decl_bndr of
        -- If the next decl_bndr is required, then we must match it against the
        -- visible forall–bound variable in the kind.
        BndrReq -> do
          let (subst', sig_tvb) = match_tvbs subst vis_tvb decl_bndr
          sig_args' <- go_vis_tvbs subst' vis_tvbs sig_args decl_bndrs
          pure ((Required <$ sig_tvb) : sig_args')
        -- We have a visible forall in the kind, but an invisible @-binder as
        -- the next decl_bndr. This is ill kinded, so throw an error.
        BndrInvis ->
          fail $ "matchUpSigWithDecl.go_vis_tvbs: Expected visible binder, encountered invisible binder: "
              ++ show decl_bndr

    -- @match_tvbs subst sig_tvb decl_bndr@ will match the kind of @decl_bndr@
    -- against the kind of @sig_tvb@ to produce a new kind. This function
    -- produces two values as output:
    --
    -- 1. A new @subst@ that has been extended such that the name of @sig_tvb@
    --    maps to the name of @decl_bndr@. (See the Haddocks for the 'DSubst'
    --    argument to @go_fun_args@ for an explanation of why we do this.)
    --
    -- 2. A 'DTyVarBndrSpec' that has the name of @decl_bndr@, but with the new
    --    kind resulting from matching.
    match_tvbs :: DSubst -> DTyVarBndr flag -> DTyVarBndrVis -> (DSubst, DTyVarBndr flag)
    match_tvbs subst sig_tvb decl_bndr =
      let decl_bndr_name = extractTvbName decl_bndr
          mb_decl_bndr_kind = extractTvbKind decl_bndr

          sig_tvb_name = extractTvbName sig_tvb
          sig_tvb_flag = extractTvbFlag sig_tvb
          mb_sig_tvb_kind = substType subst <$> extractTvbKind sig_tvb

          mb_kind :: Maybe DKind
          mb_kind =
            case (mb_decl_bndr_kind, mb_sig_tvb_kind) of
              (Nothing,             Nothing)           -> Nothing
              (Just decl_bndr_kind, Nothing)           -> Just decl_bndr_kind
              (Nothing,             Just sig_tvb_kind) -> Just sig_tvb_kind
              (Just decl_bndr_kind, Just sig_tvb_kind) -> do
                match_subst <- matchTy NoIgnore decl_bndr_kind sig_tvb_kind
                Just $ substType match_subst decl_bndr_kind

          subst' = Map.insert sig_tvb_name (DVarT decl_bndr_name) subst
          sig_tvb' = case mb_kind of
            Nothing   -> DPlainTV  decl_bndr_name sig_tvb_flag
            Just kind -> DKindedTV decl_bndr_name sig_tvb_flag kind in

      (subst', sig_tvb')

-- This is heavily inspired by the `swizzleTcb` function in GHC:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/cec903899234bf9e25ea404477ba846ac1e963bb/compiler/GHC/Tc/Gen/HsType.hs#L2741-2755
swizzleTvb :: Map Name Name -> DSubst -> DTyVarBndr flag -> (DSubst, DTyVarBndr flag)
swizzleTvb swizzle_env subst tvb =
  (subst', tvb2)
  where
    subst' = Map.insert tvb_name (DVarT (extractTvbName tvb2)) subst
    tvb_name = extractTvbName tvb
    tvb1 = mapDTVKind (substType subst) tvb
    tvb2 =
      case Map.lookup tvb_name swizzle_env of
        Just user_name -> mapDTVName (const user_name) tvb1
        Nothing        -> tvb1

-- The visibility of a binder in a type-level declaration. This generalizes
-- 'Specificity' (which lacks an equivalent to 'Required') and 'BndrVis' (which
-- lacks an equivalent to @'Invisible' 'Inferred'@).
--
-- This is heavily inspired by a data type of the same name in GHC:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/98597ad5fca81544d74f721fb508295fd2650232/compiler/GHC/Types/Var.hs#L458-465
data ForAllTyFlag
  = Invisible !Specificity
    -- ^ If the 'Specificity' value is 'SpecifiedSpec', then the binder is
    -- permitted by request (e.g., @\@a@). If the 'Specificity' value is
    -- 'InferredSpec', then the binder is prohibited from appearing in source
    -- Haskell (e.g., @\@{a}@).
  | Required
    -- ^ The binder is required to appear in source Haskell (e.g., @a@).

-- | Convert a list of @'DTyVarBndr' 'ForAllTyFlag'@s to a list of
-- 'DTyVarBndrSpec's, which is suitable for use in an invisible @forall@.
-- Specifically:
--
-- * Variable binders that use @'Invisible' spec@ are converted to @spec@.
--
-- * Variable binders that are 'Required' are converted to 'SpecifiedSpec',
--   as all of the 'DTyVarBndrSpec's are invisible. As an example of how this
--   is used, consider what would happen when singling this data type:
--
--   @
--   type T :: forall k -> k -> Type
--   data T k (a :: k) where ...
--   @
--
--   Here, the @k@ binder is 'Required'. When we produce the standalone kind
--   signature for the singled data type, we use 'tvbForAllTyFlagsToSpecs' to
--   produce the type variable binders in the outermost @forall@:
--
--   @
--   type ST :: forall k (a :: k). T k a -> Type
--   data ST z where ...
--   @
--
--   Note that the @k@ is bound visibily (i.e., using 'SpecifiedSpec') in the
--   outermost, invisible @forall@.
tvbForAllTyFlagsToSpecs :: [DTyVarBndr ForAllTyFlag] -> [DTyVarBndrSpec]
tvbForAllTyFlagsToSpecs = map (fmap to_spec)
  where
   to_spec :: ForAllTyFlag -> Specificity
   to_spec (Invisible spec) = spec
   to_spec Required         = SpecifiedSpec

-- | Convert a list of @'DTyVarBndr' 'ForAllTyFlag'@s to a list of
-- 'DTyVarBndrVis'es, which is suitable for use in a type-level declaration
-- (e.g., the @var_1 ... var_n@ in @class C var_1 ... var_n@). Specifically:
--
-- * Variable binders that use @'Invisible' 'InferredSpec'@ are dropped
--   entirely. Such binders cannot be represented in source Haskell.
--
-- * Variable binders that use @'Invisible' 'SpecifiedSpec'@ are converted to
--   'BndrInvis'.
--
-- * Variable binders that are 'Required' are converted to 'BndrReq'.
--
-- See also 'tvbSpecsToBndrVis', which takes a list of 'DTyVarBndrSpec's as
-- arguments instead of a list of @'DTyVarBndr' 'ForAllTyFlag'@s. Note that
-- @'tvbSpecsToBndrVis' . 'tvbForAllTyFlagsToSpecs'@ is /not/ the same thing as
-- 'tvbForAllTyFlagsToBndrVis'. This is because 'tvbSpecsToBndrVis' only
-- produces 'BndrInvis' binders as output, whereas 'tvbForAllTyFlagsToBndrVis'
-- can produce both 'BndrReq' and 'BndrInvis' binders.
tvbForAllTyFlagsToBndrVis :: [DTyVarBndr ForAllTyFlag] -> [DTyVarBndrVis]
tvbForAllTyFlagsToBndrVis = catMaybes . map (traverse to_spec_maybe)
  where
    to_spec_maybe :: ForAllTyFlag -> Maybe BndrVis
    to_spec_maybe (Invisible InferredSpec) = Nothing
    to_spec_maybe (Invisible SpecifiedSpec) = Just BndrInvis
    to_spec_maybe Required = Just BndrReq
