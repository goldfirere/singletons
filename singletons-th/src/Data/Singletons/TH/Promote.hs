{- Data/Singletons/TH/Promote.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains functions to promote term-level constructs to the
type level. It is an internal module to the singletons-th package.
-}

module Data.Singletons.TH.Promote where

import Language.Haskell.TH hiding ( Q, cxt )
import Language.Haskell.TH.Syntax ( NameSpace(..), Quasi(..), Uniq )
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OMap.Strict as OMap
import Language.Haskell.TH.Desugar.OMap.Strict (OMap)
import qualified Language.Haskell.TH.Desugar.OSet as OSet
import Language.Haskell.TH.Desugar.OSet (OSet)
import Data.Singletons.TH.Deriving.Bounded
import Data.Singletons.TH.Deriving.Enum
import Data.Singletons.TH.Deriving.Eq
import Data.Singletons.TH.Deriving.Ord
import Data.Singletons.TH.Deriving.Show
import Data.Singletons.TH.Deriving.Util
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Partition
import Data.Singletons.TH.Promote.Defun
import Data.Singletons.TH.Promote.Monad
import Data.Singletons.TH.Promote.Type
import Data.Singletons.TH.Syntax
import Data.Singletons.TH.Util
import Prelude hiding (exp)
import Control.Applicative (Alternative(..))
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Function (on)
import Data.List (deleteFirstsBy, nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import Data.Maybe
import qualified GHC.LanguageExtensions.Type as LangExt

{-
Note [Disable genQuotedDecs in genPromotions and genSingletons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Somewhat curiously, the genPromotions and genSingletons functions set the
genQuotedDecs option to False, despite neither function accepting quoted
declarations as arguments in the first place. There is a good reason for doing
this, however. Imagine this code:

  class C a where
    infixl 9 <%%>
    (<%%>) :: a -> a -> a
  $(genPromotions [''C])

If genQuotedDecs is set to True, then the (<%%>) type family will not receive
a fixity declaration (see
Note [singletons-th and fixity declarations] in D.S.TH.Single.Fixity, wrinkle 1 for
more details on this point). Therefore, we set genQuotedDecs to False to avoid
this problem.
-}

-- | Generate promoted definitions for each of the provided type-level
-- declaration 'Name's. This is generally only useful with classes.
genPromotions :: OptionsMonad q => [Name] -> q [Dec]
genPromotions names = do
  opts <- getOptions
  -- See Note [Disable genQuotedDecs in genPromotions and genSingletons]
  withOptions opts{genQuotedDecs = False} $ do
    checkForRep names
    infos <- mapM reifyWithLocals names
    dinfos <- mapM dsInfo infos
    ddecs <- promoteM_ [] $ mapM_ promoteInfo dinfos
    return $ decsToTH ddecs

-- | Promote every declaration given to the type level, retaining the originals.
-- See the
-- @<https://github.com/goldfirere/singletons/blob/master/README.md README>@
-- for further explanation.
promote :: OptionsMonad q => q [Dec] -> q [Dec]
promote qdecs = do
  opts <- getOptions
  withOptions opts{genQuotedDecs = True} $ promote' $ lift qdecs

-- | Promote each declaration, discarding the originals. Note that a promoted
-- datatype uses the same definition as an original datatype, so this will
-- not work with datatypes. Classes, instances, and functions are all fine.
promoteOnly :: OptionsMonad q => q [Dec] -> q [Dec]
promoteOnly qdecs = do
  opts <- getOptions
  withOptions opts{genQuotedDecs = False} $ promote' $ lift qdecs

-- The workhorse for 'promote' and 'promoteOnly'. The difference between the
-- two functions is whether 'genQuotedDecs' is set to 'True' or 'False'.
promote' :: OptionsMonad q => q [Dec] -> q [Dec]
promote' qdecs = do
  opts     <- getOptions
  decs     <- qdecs
  ddecs    <- withLocalDeclarations decs $ dsDecs decs
  promDecs <- promoteM_ decs $ promoteDecs ddecs
  let origDecs | genQuotedDecs opts = decs
               | otherwise          = []
  return $ origDecs ++ decsToTH promDecs

-- | Generate defunctionalization symbols for each of the provided type-level
-- declaration 'Name's. See the "Promotion and partial application" section of
-- the @singletons@
-- @<https://github.com/goldfirere/singletons/blob/master/README.md README>@
-- for further explanation.
genDefunSymbols :: OptionsMonad q => [Name] -> q [Dec]
genDefunSymbols names = do
  checkForRep names
  infos <- mapM (dsInfo <=< reifyWithLocals) names
  decs <- promoteMDecs [] $ concatMapM defunInfo infos
  return $ decsToTH decs

-- | Produce instances for @PEq@ from the given types
promoteEqInstances :: OptionsMonad q => [Name] -> q [Dec]
promoteEqInstances = concatMapM promoteEqInstance

-- | Produce an instance for @PEq@ from the given type
promoteEqInstance :: OptionsMonad q => Name -> q [Dec]
promoteEqInstance = promoteInstance mkEqInstance "Eq"

-- | Produce instances for 'POrd' from the given types
promoteOrdInstances :: OptionsMonad q => [Name] -> q [Dec]
promoteOrdInstances = concatMapM promoteOrdInstance

-- | Produce an instance for 'POrd' from the given type
promoteOrdInstance :: OptionsMonad q => Name -> q [Dec]
promoteOrdInstance = promoteInstance mkOrdInstance "Ord"

-- | Produce instances for 'PBounded' from the given types
promoteBoundedInstances :: OptionsMonad q => [Name] -> q [Dec]
promoteBoundedInstances = concatMapM promoteBoundedInstance

-- | Produce an instance for 'PBounded' from the given type
promoteBoundedInstance :: OptionsMonad q => Name -> q [Dec]
promoteBoundedInstance = promoteInstance mkBoundedInstance "Bounded"

-- | Produce instances for 'PEnum' from the given types
promoteEnumInstances :: OptionsMonad q => [Name] -> q [Dec]
promoteEnumInstances = concatMapM promoteEnumInstance

-- | Produce an instance for 'PEnum' from the given type
promoteEnumInstance :: OptionsMonad q => Name -> q [Dec]
promoteEnumInstance = promoteInstance mkEnumInstance "Enum"

-- | Produce instances for 'PShow' from the given types
promoteShowInstances :: OptionsMonad q => [Name] -> q [Dec]
promoteShowInstances = concatMapM promoteShowInstance

-- | Produce an instance for 'PShow' from the given type
promoteShowInstance :: OptionsMonad q => Name -> q [Dec]
promoteShowInstance = promoteInstance mkShowInstance "Show"

promoteInstance :: OptionsMonad q => DerivDesc q -> String -> Name -> q [Dec]
promoteInstance mk_inst class_name name = do
  (df, tvbs, cons) <- getDataD ("I cannot make an instance of " ++ class_name
                                ++ " for it.") name
  tvbs' <- mapM dsTvbVis tvbs
  let data_ty   = foldTypeTvbs (DConT name) tvbs'
  cons' <- concatMapM (dsCon tvbs' data_ty) cons
  let data_decl = DataDecl df name tvbs' cons'
  raw_inst <- mk_inst Nothing data_ty data_decl
  decs <- promoteM_ [] $ void $
          promoteInstanceDec OMap.empty Map.empty raw_inst
  return $ decsToTH decs

promoteInfo :: DInfo -> PrM ()
promoteInfo (DTyConI dec _instances) = promoteDecs [dec]
promoteInfo (DPrimTyConI _name _numArgs _unlifted) =
  fail "Promotion of primitive type constructors not supported"
promoteInfo (DVarI _name _ty _mdec) =
  fail "Promotion of individual values not supported"
promoteInfo (DTyVarI _name _ty) =
  fail "Promotion of individual type variables not supported"
promoteInfo (DPatSynI {}) =
  fail "Promotion of pattern synonyms not supported"

-- Promote a list of top-level declarations.
promoteDecs :: [DDec] -> PrM ()
promoteDecs raw_decls = do
  decls <- expand raw_decls     -- expand type synonyms
  checkForRepInDecls decls
  PDecs { pd_let_decs                = let_decs
        , pd_class_decs              = classes
        , pd_instance_decs           = insts
        , pd_data_decs               = datas
        , pd_ty_syn_decs             = ty_syns
        , pd_open_type_family_decs   = o_tyfams
        , pd_closed_type_family_decs = c_tyfams } <- partitionDecs decls

  defunTopLevelTypeDecls ty_syns c_tyfams o_tyfams
  rec_sel_let_decs <- promoteDataDecs datas
    -- promoteLetDecs returns LetBinds, which we don't need at top level
  _ <- promoteLetDecs Nothing $ rec_sel_let_decs ++ let_decs
  mapM_ promoteClassDec classes
  let orig_meth_sigs = foldMap (lde_types . cd_lde) classes
      cls_tvbs_map   = Map.fromList $ map (\cd -> (cd_name cd, cd_tvbs cd)) classes
  mapM_ (promoteInstanceDec orig_meth_sigs cls_tvbs_map) insts

-- curious about ALetDecEnv? See the LetDecEnv module for an explanation.
promoteLetDecs :: Maybe Uniq -- let-binding unique (if locally bound)
               -> [DLetDec] -> PrM ([LetBind], ALetDecEnv)
promoteLetDecs mb_let_uniq decls = do
  opts <- getOptions
  let_dec_env <- buildLetDecEnv decls
  all_locals <- allLocals
  let binds = [ (name, foldType (DConT sym) (map DVarT all_locals))
              | (name, _) <- OMap.assocs $ lde_defns let_dec_env
              , let proName = promotedValueName opts name mb_let_uniq
                    sym = defunctionalizedName0 opts proName ]
  (decs, let_dec_env') <- letBind binds $ promoteLetDecEnv mb_let_uniq let_dec_env
  emitDecs decs
  return (binds, let_dec_env' { lde_proms = OMap.fromList binds })

promoteDataDecs :: [DataDecl] -> PrM [DLetDec]
promoteDataDecs = concatMapM promoteDataDec

-- "Promotes" a data type, much like D.S.TH.Single.Data.singDataD singles a data
-- type. Promoting a data type is much easier than singling it, however, since
-- DataKinds automatically promotes data types and kinds and data constructors
-- to types. That means that promoteDataDec only has to do three things:
--
-- 1. Emit defunctionalization symbols for each data constructor,
--
-- 2. Emit promoted fixity declarations for each data constructor and promoted
--    record selector (assuming the originals have fixity declarations), and
--
-- 3. Assemble a top-level function that mimics the behavior of its record
--    selectors. Note that promoteDataDec does not actually promote this record
--    selector function—it merely returns its DLetDecs. Later, the promoteDecs
--    function takes these DLetDecs and promotes them (using promoteLetDecs).
--    This greatly simplifies the plumbing, since this allows all DLetDecs to
--    be promoted in a single location.
--    See Note [singletons-th and record selectors] in D.S.TH.Single.Data.
--
-- Note that if @NoFieldSelectors@ is active, then neither steps (2) nor (3)
-- will promote any records to top-level field selectors.
promoteDataDec :: DataDecl -> PrM [DLetDec]
promoteDataDec (DataDecl _ _ _ ctors) = do
  let rec_sel_names = nub $ concatMap extractRecSelNames ctors
                      -- Note the use of nub: the same record selector name can
                      -- be used in multiple constructors!
  fld_sels         <- qIsExtEnabled LangExt.FieldSelectors
  rec_sel_let_decs <- if fld_sels then getRecordSelectors ctors else pure []
  ctorSyms         <- buildDefunSymsDataD ctors
  -- NB: If NoFieldSelectors is active, then promoteReifiedInfixDecls will not
  -- promote any of `rec_sel_names` to field selectors, so there is no need to
  -- check for it here.
  infix_decs       <- promoteReifiedInfixDecls rec_sel_names
  emitDecs $ ctorSyms ++ infix_decs
  pure rec_sel_let_decs

promoteClassDec :: UClassDecl -> PrM AClassDecl
promoteClassDec decl@(ClassDecl { cd_name = cls_name
                                , cd_tvbs = orig_cls_tvbs
                                , cd_fds  = fundeps
                                , cd_atfs = atfs
                                , cd_lde  = lde@LetDecEnv
                                    { lde_defns = defaults
                                    , lde_types = meth_sigs
                                    , lde_infix = infix_decls } }) = do
  opts <- getOptions
  let pClsName       = promotedClassName opts cls_name
      meth_sigs_list = OMap.assocs meth_sigs
      meth_names     = map fst meth_sigs_list
      defaults_list  = OMap.assocs defaults
      defaults_names = map fst defaults_list
      cls_tvb_names  = map extractTvbName orig_cls_tvbs
  mb_cls_sak <- dsReifyType cls_name

  -- If the class has a standalone kind signature, we take the original,
  -- user-written class binders (`orig_cls_tvbs`) and fill them out using
  -- `matchUpSAKWithDecl` to produce the "full" binders, as described in
  -- Note [Propagating kind information from class standalone kind signatures].
  mb_full_cls_tvbs <-
    traverse (\cls_sak -> matchUpSAKWithDecl cls_sak orig_cls_tvbs) mb_cls_sak

  sig_decs <- mapM (uncurry (promote_sig mb_full_cls_tvbs)) meth_sigs_list
  (default_decs, ann_rhss, prom_rhss)
    <- mapAndUnzip3M (promoteMethod DefaultMethods meth_sigs cls_tvb_names) defaults_list
  defunAssociatedTypeFamilies orig_cls_tvbs atfs

  infix_decls' <- mapMaybeM (\(n, (f, ns)) -> promoteInfixDecl Nothing n f ns) $
                  OMap.assocs infix_decls
  cls_infix_decls <- promoteReifiedInfixDecls $ cls_name:meth_names

  -- no need to do anything to the fundeps. They work as is!
  let pro_cls_dec = DClassD [] pClsName orig_cls_tvbs fundeps
                            (sig_decs ++ default_decs ++ infix_decls')
      mb_pro_cls_sak = fmap (DKiSigD pClsName) mb_cls_sak
  emitDecs $ maybeToList mb_pro_cls_sak ++ pro_cls_dec:cls_infix_decls
  let defaults_list' = zip defaults_names ann_rhss
      proms          = zip defaults_names prom_rhss
  return (decl { cd_lde = lde { lde_defns = OMap.fromList defaults_list'
                              , lde_proms = OMap.fromList proms } })
  where
    -- Promote a class method's type signature to an associated type family.
    promote_sig ::
         Maybe [DTyVarBndr ForAllTyFlag]
         -- ^ If the parent class has a standalone kind signature, then this
         -- will be @'Just' full_bndrs@, where @full_bndrs@ are the full type
         -- variable binders described in @Note [Propagating kind information
         -- from class standalone kind signatures]@. Otherwise, this will be
         -- 'Nothing'.
      -> Name
         -- ^ The class method's name.
      -> DType
         -- ^ The class method's type.
      -> PrM DDec
         -- ^ The associated type family for the promoted class method.
    promote_sig mb_full_cls_tvbs name meth_ty = do
      opts <- getOptions
      let proName = promotedTopLevelValueName opts name
      (_, meth_arg_kis, meth_res_ki) <- promoteUnraveled meth_ty
      args <- mapM (const $ qNewName "arg") meth_arg_kis
      let pro_meth_args = zipWith (`DKindedTV` BndrReq) args meth_arg_kis
          -- Binders for all of the type variables mentioned in the argument and
          -- result kinds of the promoted class method. This includes both class
          -- variables and variables that only scope over the method itself.
          --
          -- This quantifies the variables in a simple left-to-right order,
          -- which may not be the same order in which the original method's type
          -- quantifies them. This is a known limitation: see
          -- Note [Promoted class methods and kind variable ordering].
          meth_tvbs = changeDTVFlags SpecifiedSpec $
                      toposortTyVarsOf $ meth_arg_kis ++ [meth_res_ki]
          -- The type variable binders to use in the standalone kind signatures
          -- for the promoted class method's defunctionalization symbols.
          meth_sak_tvbs =
            case tvbForAllTyFlagsToSpecs <$> mb_full_cls_tvbs of
              -- If the parent class has a standalone kind signature, then
              -- propagate as much of the kind information as possible by
              -- incorporating the full class binders. See Note [Propagating
              -- kind information from class standalone kind signatures].
              Just full_cls_tvbs_spec ->
                -- `meth_tvbs` can include class binder names, so make sure to
                -- delete type variables from `meth_tvbs` whose names are also
                -- bound by the full class binders.
                let meth_tvbs_without_cls_tvbs =
                      deleteFirstsBy
                        ((==) `on` extractTvbName)
                        meth_tvbs
                        full_cls_tvbs_spec in
                full_cls_tvbs_spec ++ meth_tvbs_without_cls_tvbs
              -- If the parent class lacks a standalone kind signature, then we
              -- simply return `meth_tvbs`.
              Nothing ->
                meth_tvbs
          meth_sak = ravelVanillaDType meth_sak_tvbs [] meth_arg_kis meth_res_ki
      m_fixity <- reifyFixityWithLocals name
      emitDecsM $ defunctionalize proName m_fixity $ DefunSAK meth_sak

      return $ DOpenTypeFamilyD (DTypeFamilyHead proName
                                                 pro_meth_args
                                                 (DKindSig meth_res_ki)
                                                 Nothing)

{-
Note [Promoted class methods and kind variable ordering]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, we make an effort to preserve the order of type variables when
promoting type signatures, but there is an annoying corner case where this is
difficult: class methods. When promoting class methods, the order of kind
variables in their kinds will often "just work" by happy coincidence, but
there are some situations where this does not happen. Consider the following
class:

  class C (b :: Type) where
    m :: forall a. a -> b -> a

The full type of `m` is `forall b. C b => forall a. a -> b -> a`, which binds
`b` before `a`. This order is preserved when singling `m`, but *not* when
promoting `m`. This is because the `C` class is promoted as follows:

  class PC (b :: Type) where
    type M (x :: a) (y :: b) :: a

Due to the way GHC kind-checks associated type families, the kind of `M` is
`forall a b. a -> b -> a`, which binds `b` *after* `a`. Moreover, the
`StandaloneKindSignatures` extension does not provide a way to explicitly
declare the full kind of an associated type family, so this limitation is
not easy to work around.

The defunctionalization symbols for `M` will also follow a similar
order of type variables:

  type MSym0 :: forall a b. a ~> b ~> a
  type MSym1 :: forall a b. a -> b ~> a

In the past, we have considered different ways to rectify this, but none of
the approaches that we have tried are quite satisfactory:

* We could hackily specify the order of kind variables using a type synonym
  like `FlipConst`:

    type FlipConst x y = y
    class PC (b :: Type) where
      type M (x :: FlipConst '(b, a) a) (y :: b) :: a

  Using `FlipConst` would cause `b` to be mentioned before `a`, which would give
  `M` the kind `forall b a. FlipConst '(b, a) a -> b -> a`. While the order of
  type variables would be preserved, the downside is that the ugly `FlipConst`
  type synonym leaks into the kind. I'm not particularly fond of this, so I have
  decided not to use this hack unless someone specifically requests it.

* We could specify the order of kind variables using the TypeAbstractions
  language extension:

    class PC (b :: Type) where
      type M @(b :: Type) @a (x :: a) (y :: b) :: a

  This is much nicer to look at than the `FlipConst` hack above. However, this
  approach has its own drawbacks. For one thing, GHC only permits using
  TypeAbstractions in an associated type family declaration if its parent class
  also has a standalone kind signature. As such, this trick would only work some
  of the time.

  Even if we /did/ give the parent class a standalone kind signature, however,
  it is still not guaranteed that the promoted method would kind-check. Consider
  what would happen if you promoted this class:

    type Traversable :: (Type -> Type) -> Constraint
    class (Functor t, Foldable t) => Traversable t where
      traverse :: Applicative f => (a -> f b) -> t a -> t (f b)

  This would be promoted to:

    type PTraversable :: (Type -> Type) -> Constraint
    class PTraversable t where
      type PTraverse @(t :: Type -> Type) @f @a @b
                     (x :: a ~> f b) (y :: t a) :: t (f b)

  There is a subtle problem with this definition: because the `@f` binder lacks
  an explicit kind signature, GHC defaults its kind to `Type`. This is very bad,
  however, because `f`'s kind must be `Type -> Type`, not `Type`! Nor would it
  be straightforward to generate `@(f :: Type -> Type)`, as nothing in the
  original definition of `traverse` explicitly indicates that `f` has the kind
  `Type -> Type`.

  In theory, we could implement kind inference inside of Template Haskell to
  infer that `f :: Type -> Type`, but this is a tall order. Best to keep things
  simple and not do this.

Note [Propagating kind information from class standalone kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider what happens when you promote this example:

  type Alternative :: (Type -> Type) -> Constraint
  class Applicative f => Alternative f where
    empty :: f a
    ...

We want the promoted `Empty` type family, as well as the `EmptySym0`
defunctionalization symbol, to have the kind
`forall (f :: Type -> Type) a. f a`. Giving `Empty` the appropriate kind is
easy enough, as we can simply copy over `Alternative`'s standalone kind
signature to `PAltenative`, its promoted counterpart:

  type PAlternative :: (Type -> Type) -> Constraint
  class PAlternative f where
    type Empty :: f a
    ...

Giving `EmptySym0` the appropriate kind is trickier, however. A naïve approach
would be to generate this:

  type EmptySym0 :: f a
  type family EmptySym0 where
    EmptySym0 = Empty

This would give EmptySym0 a more general kind than what we want, however, as
GHC will generalize this to:

  EmptySym0 :: forall {k} (f :: k -> Type) (a :: k). f a

This is undesirable, as now `EmptySym0` can be called on kinds that cannot have
`PAlternative` instances. What's more, if GHC proposal #425 were fully
implemented (see
https://github.com/ghc-proposals/ghc-proposals/blob/8443acc903437cef1a7fbb56de79b6dce77b1a09/proposals/0425-decl-invis-binders.rst#proposed-change-specification-instances),
then this code would simply not kind-check, as the left-hand side of the
`EmptySym0 = Empty` would be too general for its right-hand side.

Instead, we strive to generate this code for `EmptySym0` instead:

  type EmptySym0 :: forall (f :: Type -> Type) a. f a
  type family EmptySym0 where
    EmptySym0 = Empty

This is very doable because the user gave `Alternative` a standalone kind
signature, so it should be possible to match up the `Type -> Type` part of the
standalone kind signature with `f`. And that is exactly what we do:

* In `promoteClassDec`, we use the `matchUpSAKWithDecl` function to take the
  original class type variable binders and the class standalone kind signature
  as input and produce a new set of class binders as output, where the new
  binders have been annotated with kinds taken from the standalone kind
  signature. We will call these new class type variable binders the /full/
  binders.

* When generating a defunctionalization symbol for a promoted class method, we
  always quantify the defunctionalization symbol's kind using an explicit
  `forall`, where the `forall` looks like:

    forall <full class type variable binders> <method type variable binders>. ...

  This ensures that the kind information from the full class binders is
  propagated through to the defunctionalization symbol. (Note that we do not
  make any guarantees about the /order/ of these type variables, however. See
  Note [Promoted class methods and kind variable ordering].)

If the parent class lacks a standalone kind signature, then we skip all of this
and simply quantify the the defunctionalization symbols' kind variables in a
left-to-right order. Again, the order of these kind variables in unspecified, so
we are free to choose a simpler implementation that makes our lives easier.
-}

-- returns (unpromoted method name, ALetDecRHS) pairs
promoteInstanceDec :: OMap Name DType
                      -- Class method type signatures
                   -> Map Name [DTyVarBndrVis]
                      -- Class header type variable (e.g., if `class C a b` is
                      -- quoted, then this will have an entry for {C |-> [a, b]})
                   -> UInstDecl -> PrM AInstDecl
promoteInstanceDec orig_meth_sigs cls_tvbs_map
                   decl@(InstDecl { id_name     = cls_name
                                  , id_arg_tys  = inst_tys
                                  , id_sigs     = inst_sigs
                                  , id_meths    = meths }) = do
  opts <- getOptions
  cls_tvbs <- lookup_cls_tvbs
  inst_kis <- mapM promoteType inst_tys
  let pClsName      = promotedClassName opts cls_name
      cls_tvb_names = map extractTvbName cls_tvbs
      subst         = Map.fromList $ zip cls_tvb_names inst_kis
      meth_impl     = InstanceMethods inst_sigs subst
      inst_ki_fvs   = map extractTvbName $ toposortTyVarsOf inst_kis
  (meths', ann_rhss, _)
    <- mapAndUnzip3M (promoteMethod meth_impl orig_meth_sigs inst_ki_fvs) meths
  emitDecs [DInstanceD Nothing Nothing [] (foldType (DConT pClsName)
                                            inst_kis) meths']
  return (decl { id_meths = zip (map fst meths) ann_rhss })
  where
    lookup_cls_tvbs :: PrM [DTyVarBndrVis]
    lookup_cls_tvbs =
      -- First, try consulting the map of class names to their type variables.
      -- It is important to do this first to ensure that we consider locally
      -- declared classes before imported ones. See #410 for what happens if
      -- you don't.
      case Map.lookup cls_name cls_tvbs_map of
        Just tvbs -> pure tvbs
        Nothing   -> reify_cls_tvbs
          -- If the class isn't present in this map, we try reifying the class
          -- as a last resort.

    reify_cls_tvbs :: PrM [DTyVarBndrVis]
    reify_cls_tvbs = do
      opts <- getOptions
      let pClsName = promotedClassName opts cls_name
          mk_tvbs  = extract_tvbs (dsReifyTypeNameInfo pClsName)
                 <|> extract_tvbs (dsReifyTypeNameInfo cls_name)
                      -- See Note [Using dsReifyTypeNameInfo when promoting instances]
      mb_tvbs <- runMaybeT mk_tvbs
      case mb_tvbs of
        Just tvbs -> pure tvbs
        Nothing -> fail $ "Cannot find class declaration annotation for " ++ show cls_name

    extract_tvbs :: PrM (Maybe DInfo) -> MaybeT PrM [DTyVarBndrVis]
    extract_tvbs reify_info = do
      mb_info <- lift reify_info
      case mb_info of
        Just (DTyConI (DClassD _ _ tvbs _ _) _) -> pure tvbs
        _                                       -> empty

{-
Note [Using dsReifyTypeNameInfo when promoting instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During the promotion of a class instance, it becomes necessary to reify the
original promoted class's info to learn various things. It's tempting to think
that just calling dsReify on the class name will be sufficient, but it's not.
Consider this class and its promotion:

  class Eq a where
    (==) :: a -> a -> Bool

  class PEq a where
    type (==) (x :: a) (y :: a) :: Bool

Notice how both of these classes have an identifier named (==), one at the
value level, and one at the type level. Now imagine what happens when you
attempt to promote this Template Haskell declaration:

   [d| f :: Bool
       f = () == () |]

When promoting ==, singletons-th will come up with its promoted equivalent (which also
happens to be ==). However, this promoted name is a raw Name, since it is created
with mkName. This becomes an issue when we call dsReify the raw "==" Name, as
Template Haskell has to arbitrarily choose between reifying the info for the
value-level (==) and the type-level (==), and in this case, it happens to pick the
value-level (==) info. We want the type-level (==) info, however, because we care
about the promoted version of (==).

Fortunately, there's a serviceable workaround. Instead of dsReify, we can use
dsReifyTypeNameInfo, which first calls lookupTypeName (to ensure we can find a Name
that's in the type namespace) and _then_ reifies it.
-}

-- Which sort of class methods are being promoted?
data MethodSort
    -- The method defaults in class declarations.
  = DefaultMethods
    -- The methods in instance declarations.
  | InstanceMethods (OMap Name DType) -- ^ InstanceSigs
                    (Map Name DKind)  -- ^ Instantiations for class tyvars
                                      --   See Note [Promoted class method kinds]
  deriving Show

promoteMethod :: MethodSort
              -> OMap Name DType    -- method types
              -> [Name] -- The names of the type variables bound by the class
                        -- header (e.g., the @a@ in @class C a where ...@).
              -> (Name, ULetDecRHS)
              -> PrM (DDec, ALetDecRHS, DType)
                 -- returns (type instance, ALetDecRHS, promoted RHS)
promoteMethod meth_sort orig_sigs_map cls_tvb_names (meth_name, meth_rhs) = do
  opts <- getOptions
  (meth_tvbs, meth_arg_kis, meth_res_ki) <- promote_meth_ty
  -- If ScopedTypeVariables is enabled, bring type variables into scope over the
  -- RHS. These type variables can come from the class/instance header, the
  -- method type signature/instance signature, or both, depending on how the
  -- class or instance declaration is written. See
  -- Note [Scoped type variables and class methods] in D.S.TH.Promote.Monad.
  -- See also (Wrinkle: Partial scoping) from that Note for a scenario in which
  -- we bring class/instance header type variables into scope but /not/
  -- type variables from the class method/instance signature.
  scoped_tvs_ext <- qIsExtEnabled LangExt.ScopedTypeVariables
  let meth_scoped_tvs
        | scoped_tvs_ext
        = OSet.fromList (cls_tvb_names ++ map extractTvbName meth_tvbs)
        | otherwise
        = OSet.empty
  meth_arg_tvs <- replicateM (length meth_arg_kis) (qNewName "a")
  let proName = promotedTopLevelValueName opts meth_name
      helperNameBase = case nameBase proName of
                         first:_ | not (isHsLetter first) -> "TFHelper"
                         alpha                            -> alpha

      -- family_args are the type variables in a promoted class's
      -- associated type family instance (or default implementation), e.g.,
      --
      --   class C k where
      --     type T (a :: k) (b :: Bool)
      --     type T a b = THelper1 a b        -- family_args = [a, b]
      --
      --   instance C Bool where
      --     type T a b = THelper2 a b        -- family_args = [a, b]
      --
      -- We could annotate these variables with explicit kinds, but it's not
      -- strictly necessary, as kind inference can figure them out just as well.
      family_args = map DVarT meth_arg_tvs
  helperName <- newUniqueName helperNameBase
  let helperDefunName = defunctionalizedName0 opts helperName
  (pro_decs, defun_decs, ann_rhs)
    <- promoteLetDecRHS (ClassMethodRHS meth_scoped_tvs meth_arg_kis meth_res_ki)
                        OMap.empty OMap.empty
                        Nothing helperName meth_rhs
  emitDecs (pro_decs ++ defun_decs)
  return ( DTySynInstD
             (DTySynEqn Nothing
                        (foldType (DConT proName) family_args)
                        (foldApply (DConT helperDefunName) (map DVarT meth_arg_tvs)))
         , ann_rhs
         , DConT helperDefunName )
  where
    -- Promote the type of a class method. For a default method, "the type" is
    -- simply the type of the original method. For an instance method,
    -- "the type" is like the type of the original method, but substituted for
    -- the types in the instance head. (e.g., if you have `class C a` and
    -- `instance C T`, then the substitution [a |-> T] must be applied to the
    -- original method's type.)
    --
    -- This returns three things in a tuple:
    --
    -- * The list of scoped type variables from the class method signature or
    --   instance signature. If an instance method lacks an instance signature,
    --   this will be an empty list. These type variables will be brought into
    --   scope over the RHS of the method: see Note [Scoped type variables and
    --   class methods] in D.S.TH.Promote.Monad.
    --
    -- * The promoted argument kinds.
    --
    -- * The promoted result kind.
    promote_meth_ty :: PrM ([DTyVarBndrSpec], [DKind], DKind)
    promote_meth_ty =
      case meth_sort of
        DefaultMethods ->
          -- No substitution for class variables is required for default
          -- method type signatures, as they share type variables with the
          -- class they inhabit.
          lookup_meth_ty
        InstanceMethods inst_sigs_map cls_subst ->
          case OMap.lookup meth_name inst_sigs_map of
            Just ty -> do
              -- We have an InstanceSig. These are easy: we can just use the
              -- instance signature's type directly, and no substitution for
              -- class variables is required.
              promoteUnraveled ty
            Nothing -> do
              -- We don't have an InstanceSig, so we must compute the kind to use
              -- ourselves.
              (_, arg_kis, res_ki) <- lookup_meth_ty
              -- Substitute for the class variables in the method's type.
              -- See Note [Promoted class method kinds]
              let arg_kis' = map (substKind cls_subst) arg_kis
                  res_ki'  = substKind cls_subst res_ki
              -- If there is no instance signature, then there are no additional
              -- type variables to bring into scope, so return an empty list of
              -- scoped type variables.
              pure ([], arg_kis', res_ki')

    -- Attempt to look up a class method's original type.
    lookup_meth_ty :: PrM ([DTyVarBndrSpec], [DKind], DKind)
    lookup_meth_ty = do
      opts <- getOptions
      let proName = promotedTopLevelValueName opts meth_name
      case OMap.lookup meth_name orig_sigs_map of
        Just ty -> do
          -- The type of the method is in scope, so promote that.
          promoteUnraveled ty
        Nothing -> do
          -- If the type of the method is not in scope, the only other option
          -- is to try reifying the promoted method name.
          mb_info <- dsReifyTypeNameInfo proName
                     -- See Note [Using dsReifyTypeNameInfo when promoting instances]
          case mb_info of
            Just (DTyConI (DOpenTypeFamilyD (DTypeFamilyHead _ tvbs mb_res_ki _)) _)
              -> let arg_kis = map (defaultMaybeToTypeKind . extractTvbKind) tvbs
                     res_ki  = defaultMaybeToTypeKind (resultSigToMaybeKind mb_res_ki)
                  -- If there is no instance signature, then there are no
                  -- additional type variables to bring into scope, so return an
                  -- empty list of scoped type variables.
                  in pure ([], arg_kis, res_ki)
            _ -> fail $ "Cannot find type annotation for " ++ show proName

{-
Note [Promoted class method kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example of a type class (and instance):

  class C a where
    m :: a -> Bool -> Bool
    m _ x = x

  instance C [a] where
    m l _ = null l

The promoted version of these declarations would be:

  class PC a where
    type M (x :: a) (y :: Bool) :: Bool
    type M x y = MHelper1 x y

  instance PC [a] where
    type M x y = MHelper2 x y

  type MHelper1 :: a -> Bool -> Bool
  type family MHelper1 x y where ...

  type MHelper2 :: [a] -> Bool -> Bool
  type family MHelper2 x y where ...

Getting the kind signature for MHelper1 (the promoted default implementation of
M) is quite simple, as it corresponds exactly to the kind of M. We might even
choose to make that the kind of MHelper2, but then it would be overly general
(and more difficult to find in -ddump-splices output). For this reason, we
substitute in the kinds of the instance itself to determine the kinds of
promoted method implementations like MHelper2.
-}

promoteLetDecEnv :: Maybe Uniq -> ULetDecEnv -> PrM ([DDec], ALetDecEnv)
promoteLetDecEnv mb_let_uniq (LetDecEnv { lde_defns = value_env
                                        , lde_types = type_env
                                        , lde_infix = fix_env }) = do
  infix_decls <- mapMaybeM (\(n, (f, ns)) -> promoteInfixDecl mb_let_uniq n f ns) $
                 OMap.assocs fix_env

    -- promote all the declarations, producing annotated declarations
  let (names, rhss) = unzip $ OMap.assocs value_env
  (pro_decs, defun_decss, ann_rhss)
    <- fmap unzip3 $
       zipWithM (promoteLetDecRHS LetBindingRHS type_env (fmap fst fix_env) mb_let_uniq)
                names rhss

  emitDecs $ concat defun_decss
  let decs = concat pro_decs ++ infix_decls

    -- build the ALetDecEnv
  let let_dec_env' = LetDecEnv { lde_defns     = OMap.fromList $ zip names ann_rhss
                               , lde_types     = type_env
                               , lde_infix     = fix_env
                               , lde_proms     = OMap.empty  -- filled in promoteLetDecs
                               }

  return (decs, let_dec_env')

-- Promote a fixity declaration.
promoteInfixDecl :: forall q. OptionsMonad q
                 => Maybe Uniq -> Name -> Fixity
                 -> NamespaceSpecifier
                    -- The namespace specifier for the fixity declaration. We
                    -- only pass this for the sake of checking if we need to
                    -- avoid promoting a fixity declaration (see `promote_val`
                    -- below). The actual namespace used in the promoted fixity
                    -- declaration will always be `type`.
                 -> q (Maybe DDec)
promoteInfixDecl mb_let_uniq name fixity ns = do
  opts <- getOptions
  fld_sels <- qIsExtEnabled LangExt.FieldSelectors
  mb_ns <- reifyNameSpace name
  case mb_ns of
    -- If we can't find the Name for some odd reason, fall back to promote_val
    Nothing          -> promote_val
    Just VarName     -> promote_val
    Just (FldName _)
      | fld_sels     -> promote_val
      | otherwise    -> never_mind
    Just DataName    -> never_mind
    Just TcClsName   -> do
      mb_info <- dsReify name
      case mb_info of
        Just (DTyConI DClassD{} _)
          -> finish $ promotedClassName opts name
        _ -> never_mind
  where
    -- Produce the fixity declaration. Promoted names always inhabit the `type`
    -- namespace (i.e., `TypeNamespaceSpecifier`).
    finish :: Name -> q (Maybe DDec)
    finish = pure . Just . DLetDec . DInfixD fixity TypeNamespaceSpecifier

    -- Don't produce a fixity declaration at all. This can happen in the
    -- following circumstances:
    --
    -- - When promoting a fixity declaration for a name whose promoted
    --   counterpart is the same as the original name.
    --   See Note [singletons-th and fixity declarations] in
    --   D.S.TH.Single.Fixity, wrinkle 1.
    --
    -- - A fixity declaration contains the name of a record selector when
    --   NoFieldSelectors is active.
    never_mind :: q (Maybe DDec)
    never_mind = pure Nothing

    -- Certain value names do not change when promoted (e.g., infix names).
    -- Therefore, don't bother promoting their fixity declarations if the
    -- following hold:
    --
    -- - 'genQuotedDecs' is set to 'True'.
    --
    -- - The name lacks an explicit namespace specifier.
    --
    -- Doing so will run the risk of generating duplicate fixity declarations.
    -- See Note [singletons-th and fixity declarations] in D.S.TH.Single.Fixity, wrinkle 1.
    promote_val :: q (Maybe DDec)
    promote_val = do
      opts <- getOptions
      let promoted_name :: Name
          promoted_name = promotedValueName opts name mb_let_uniq
      if nameBase name == nameBase promoted_name
         && genQuotedDecs opts
         && ns == NoNamespaceSpecifier
         then never_mind
         else finish promoted_name

-- Try producing promoted fixity declarations for Names by reifying them
-- /without/ consulting quoted declarations. If reification fails, recover and
-- return the empty list.
-- See [singletons-th and fixity declarations] in D.S.TH.Single.Fixity, wrinkle 2.
promoteReifiedInfixDecls :: forall q. OptionsMonad q => [Name] -> q [DDec]
promoteReifiedInfixDecls = mapMaybeM tryPromoteFixityDeclaration
  where
    tryPromoteFixityDeclaration :: Name -> q (Maybe DDec)
    tryPromoteFixityDeclaration name =
      qRecover (return Nothing) $ do
        mFixity <- qReifyFixity name
        case mFixity of
          Nothing     -> pure Nothing
          -- NB: We don't have a NamespaceSpecifier in hand here. We could try
          -- to look one up, but it doesn't actually matter which namespace we
          -- pass here. If we reach this point in the code, we know we have a
          -- non-quoted Name (as reification would failed earlier if the Name
          -- were quoted). As such, the special case described in
          -- [singletons-th and fixity declarations] in D.S.TH.Single.Fixity,
          -- wrinkle 1 won't apply, and we only pass a namespace specifier for
          -- the sake of checking this special case.
          Just fixity -> promoteInfixDecl Nothing name fixity NoNamespaceSpecifier

-- Which sort of let-bound declaration's right-hand side is being promoted?
data LetDecRHSSort
    -- An ordinary (i.e., non-class-related) let-bound declaration.
  = LetBindingRHS
    -- The right-hand side of a class method (either a default method or a
    -- method in an instance declaration).
  | ClassMethodRHS
      (OSet Name) -- The scoped type variables to bring into scope over
                  -- the RHS of the class method. See
                  -- Note [Scoped type variables and class methods]
                  -- in D.S.TH.Promote.Monad.
      [DKind]     -- The RHS's promoted argument kinds. Needed to fix #136.
      DKind       -- The RHS's promoted result kind. Needed to fix #136.
  deriving Show

-- This function is used both to promote class method defaults and normal
-- let bindings. Thus, it can't quite do all the work locally and returns
-- an intermediate structure. Perhaps a better design is available.
promoteLetDecRHS :: LetDecRHSSort
                 -> OMap Name DType      -- local type env't
                 -> OMap Name Fixity     -- local fixity env't
                 -> Maybe Uniq           -- let-binding unique (if locally bound)
                 -> Name                 -- name of the thing being promoted
                 -> ULetDecRHS           -- body of the thing
                 -> PrM ( [DDec]        -- promoted type family dec, plus the
                                        -- SAK dec (if one exists)
                        , [DDec]        -- defunctionalization
                        , ALetDecRHS )  -- annotated RHS
promoteLetDecRHS rhs_sort type_env fix_env mb_let_uniq name let_dec_rhs = do
  all_locals <- allLocals
  case let_dec_rhs of
    UValue exp -> do
      (m_ldrki, ty_num_args) <- promote_let_dec_ty all_locals 0
      if ty_num_args == 0
      then do
        prom_fun_lhs <- promoteLetDecName mb_let_uniq name m_ldrki all_locals
        promote_let_dec_rhs all_locals m_ldrki 0 (promoteExp exp)
                            (\exp' -> [DTySynEqn Nothing prom_fun_lhs exp'])
                            AValue
      else
        -- If we have a UValue with a function type, process it as though it
        -- were a UFunction. promote_function_rhs will take care of
        -- eta-expanding arguments as necessary.
        promote_function_rhs all_locals [DClause [] exp]
    UFunction clauses -> promote_function_rhs all_locals clauses
  where
    -- Promote the RHS of a UFunction (or a UValue with a function type).
    promote_function_rhs :: [Name]
                         -> [DClause] -> PrM ([DDec], [DDec], ALetDecRHS)
    promote_function_rhs all_locals clauses = do
      numArgs <- count_args clauses
      (m_ldrki, ty_num_args) <- promote_let_dec_ty all_locals numArgs
      expClauses <- mapM (etaContractOrExpand ty_num_args numArgs) clauses
      let promote_clause = promoteClause mb_let_uniq name m_ldrki all_locals
      promote_let_dec_rhs all_locals m_ldrki ty_num_args
                          (mapAndUnzipM promote_clause expClauses)
                          id (AFunction ty_num_args)

    -- Promote a UValue or a UFunction.
    -- Notes about type variables:
    --
    -- * For UValues, `prom_a` is DType and `a` is Exp.
    --
    -- * For UFunctions, `prom_a` is [DTySynEqn] and `a` is [DClause].
    promote_let_dec_rhs
      :: [Name]                   -- Local variables bound in this scope
      -> Maybe LetDecRHSKindInfo  -- Information about the promoted kind (if present)
      -> Int                      -- The number of promoted function arguments
      -> PrM (prom_a, a)          -- Promote the RHS
      -> (prom_a -> [DTySynEqn])  -- Turn the promoted RHS into type family equations
      -> (a -> ALetDecRHS)        -- Build an ALetDecRHS
      -> PrM ([DDec], [DDec], ALetDecRHS)
    promote_let_dec_rhs all_locals m_ldrki ty_num_args
                        promote_thing mk_prom_eqns mk_alet_dec_rhs = do
      opts <- getOptions
      tyvarNames <- replicateM ty_num_args (qNewName "a")
      let proName    = promotedValueName opts name mb_let_uniq
          m_fixity   = OMap.lookup name fix_env

          mk_tf_head :: [DTyVarBndrVis] -> DFamilyResultSig -> DTypeFamilyHead
          mk_tf_head arg_tvbs res_sig =
            dTypeFamilyHead_with_locals proName all_locals arg_tvbs res_sig

          (lde_kvs_to_bind, m_sak_dec, defun_ki, tf_head) =
              -- There are three possible cases:
            case m_ldrki of
              -- 1. We have no kind information whatsoever.
              Nothing ->
                let arg_tvbs = map (`DPlainTV` BndrReq) tyvarNames in
                ( OSet.empty
                , Nothing
                , DefunNoSAK all_locals arg_tvbs Nothing
                , mk_tf_head arg_tvbs DNoSig
                )
              -- 2. We have some kind information in the form of a LetDecRHSKindInfo.
              Just (LDRKI m_sak lde_kvs_to_bind' tvbs argKs resK) ->
                let arg_tvbs = zipWith (`DKindedTV` BndrReq) tyvarNames argKs in
                case m_sak of
                  -- 2(a). We do not have a standalone kind signature.
                  Nothing ->
                    ( lde_kvs_to_bind'
                    , Nothing
                    , DefunNoSAK all_locals arg_tvbs (Just resK)
                    , mk_tf_head arg_tvbs (DKindSig resK)
                    )
                  -- 2(b). We have a standalone kind signature.
                  Just sak ->
                    -- Compute the type variable binders needed to give the type
                    -- family the correct arity.
                    -- See Note [Generating type families with the correct arity].
                    let tvbs' | null tvbs
                              = changeDTVFlags SpecifiedSpec $
                                toposortTyVarsOf (argKs ++ [resK])
                              | otherwise
                              = tvbs
                        arg_tvbs' = tvbSpecsToBndrVis tvbs' ++ arg_tvbs in
                    ( lde_kvs_to_bind'
                    , Just $ DKiSigD proName sak
                    , DefunSAK sak
                      -- We opt to annotate the argument and result kinds in
                      -- the body of the type family declaration even if it is
                      -- given a standalone kind signature.
                      -- See Note [Keep redundant kind information for Haddocks].
                    , mk_tf_head arg_tvbs' (DKindSig resK)
                    )

      defun_decs <- defunctionalize proName m_fixity defun_ki
      (prom_thing, thing) <- scopedBind lde_kvs_to_bind promote_thing
      return ( catMaybes [ m_sak_dec
                         , Just $ DClosedTypeFamilyD tf_head (mk_prom_eqns prom_thing)
                         ]
             , defun_decs
             , mk_alet_dec_rhs thing )

    promote_let_dec_ty :: [Name] -- The local variables that the let-dec closes
                                 -- over. If this is non-empty, we cannot
                                 -- produce a standalone kind signature.
                                 -- See Note [No SAKs for let-decs with local variables]
                       -> Int    -- The number of arguments to default to if the
                                 -- type cannot be inferred. This is 0 for UValues
                                 -- and the number of arguments in a single clause
                                 -- for UFunctions.
                       -> PrM (Maybe LetDecRHSKindInfo, Int)
                                 -- Returns two things in a pair:
                                 --
                                 -- 1. Information about the promoted kind,
                                 --    if available.
                                 --
                                 -- 2. The number of arguments the let-dec has.
                                 --    If no kind information is available from
                                 --    which to infer this number, then this
                                 --    will default to the earlier Int argument.
    promote_let_dec_ty all_locals default_num_args =
      case rhs_sort of
        ClassMethodRHS scoped_tvs arg_kis res_ki
          -> -- Class method RHS helper functions are only used internally, so
             -- there's no point in trying to get the order of type variables
             -- correct. Nevertheless, we /do/ want to bind the type variables
             -- in an outermost `forall` so that we can bring any scoped type
             -- variables into scope. As such, we simply quantify the type
             -- variables from left to right.
             -- See Note [Scoped type variables and class methods] in
             -- D.S.TH.Promote.Monad.
             let sak_tvbs = changeDTVFlags SpecifiedSpec $
                            toposortTyVarsOf $ arg_kis ++ [res_ki]
                 sak = ravelVanillaDType sak_tvbs [] arg_kis res_ki in
             return (Just (LDRKI (Just sak) scoped_tvs sak_tvbs arg_kis res_ki), length arg_kis)
        LetBindingRHS
          |  Just ty <- OMap.lookup name type_env
          -> do
          -- promoteType turns rank-1 uses of (->) into (~>). So, we unravel
          -- first to avoid this behavior, and then ravel back.
          (tvbs, argKs, resultK) <- promoteUnraveled ty
          let m_sak | null all_locals = Just $ ravelVanillaDType tvbs [] argKs resultK
                      -- If this let-dec closes over local variables, then
                      -- don't give it a SAK.
                      -- See Note [No SAKs for let-decs with local variables]
                    | otherwise       = Nothing
          -- If ScopedTypeVariables is enabled, bring all of the type variables
          -- from the outermost forall into scope over the RHS.
          scoped_tvs_ext <- qIsExtEnabled LangExt.ScopedTypeVariables
          let scoped_tvs | scoped_tvs_ext
                         = OSet.fromList (map extractTvbName tvbs)
                         | otherwise
                         = OSet.empty
          -- invariant: count_args ty == length argKs
          return (Just (LDRKI m_sak scoped_tvs tvbs argKs resultK), length argKs)

          |  otherwise
          -> return (Nothing, default_num_args)

    etaContractOrExpand :: Int -> Int -> DClause -> PrM DClause
    etaContractOrExpand ty_num_args clause_num_args (DClause pats exp)
      | n >= 0 = do -- Eta-expand
          names <- replicateM n (newUniqueName "a")
          let newPats = map DVarP names
              newArgs = map DVarE names
          return $ DClause (pats ++ newPats) (foldExp exp newArgs)
      | otherwise = do -- Eta-contract
          let (clausePats, lamPats) = splitAt ty_num_args pats
          lamExp <- mkDLamEFromDPats lamPats exp
          return $ DClause clausePats lamExp
      where
        n = ty_num_args - clause_num_args

    count_args :: [DClause] -> PrM Int
    count_args (DClause pats _ : _) = return $ length pats
    count_args _ = fail $ "Impossible! A function without clauses."

-- An auxiliary data type used in promoteLetDecRHS that describes information
-- related to the promoted kind of a class method default or normal
-- let binding.
data LetDecRHSKindInfo =
  LDRKI (Maybe DKind)    -- The standalone kind signature, if applicable.
                         -- This will be Nothing if the let-dec RHS has local
                         -- variables that it closes over.
                         -- See Note [No SAKs for let-decs with local variables]
        (OSet Name)      -- The scoped type variables to bring into scope over
                         -- the RHS of the let-dec. This will be a subset of the
                         -- type variables of the kind (see the field below),
                         -- but not necessarily the same. See
                         -- Note [Scoped type variables and class methods]
                         -- (Wrinkle: Partial scoping) in D.S.TH.Promote.Monad
                         -- for an example where this can be a proper subset.
        [DTyVarBndrSpec] -- The type variable binders of the kind.
        [DKind]          -- The argument kinds.
        DKind            -- The result kind.

{-
Note [No SAKs for let-decs with local variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider promoting this:

  f :: Bool
  f = let x = True
          g :: () -> Bool
          g _ = x
      in g ()

Clearly, the promoted `F` type family will have the following SAK:

  type F :: ()

What about `G`? At a passing glance, it appears that you could get away with
this:

  type G :: Bool -> ()

But this isn't quite right, since `g` closes over `x = True`. The body of `G`,
therefore, has to lift `x` to be an explicit argument:

  type family G x (u :: ()) :: Bool where
    G x _ = x

At present, we don't keep track of the types of local variables like `x`, which
makes it difficult to create a SAK for things like `G`. Here are some possible
ideas, each followed by explanations for why they are infeasible:

* Use wildcards:

    type G :: _ -> () -> Bool

  Alas, GHC currently does not allow wildcards in SAKs. See GHC#17432.

* Use visible dependent quantification to avoid having to say what the kind
  of `x` is:

    type G :: forall x -> () -> Bool

  A clever trick to be sure, but it doesn't quite do what we want, since
  GHC will generalize that kind to become `forall (x :: k) -> () -> Bool`,
  which is more general than we want.

In any case, it's probably not worth bothering with SAKs for local definitions
like `g` in the first place, so we avoid generating SAKs for anything that
closes over at least one local variable for now. If someone yells about this,
we'll reconsider this design.

Note [Keep redundant kind information for Haddocks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`singletons-th` generates explicit argument kinds and result kinds for
type-level declarations whenever possible, even if those kinds are technically
redundant. For example, `singletons-th` would promote this:

  id' :: a -> a

To this:

  type Id' :: a -> a
  type family Id' (x :: a) :: a where ...

Strictly speaking, the argument and result kind of Id' are unnecessary, since
the same information is already present in the standalone kind signature.
However, due to a Haddock limitation
(https://github.com/haskell/haddock/issues/1178), Haddock will not render
standalone kind signatures at all, so if the argument and result kind of Id'
were omitted in the body, Haddock would render it like so:

  type family Id' x where ...

This is unfortunate for Haddock viewers, as this does not convey any kind
information whatsoever. Until the aformentioned Haddock issue is resolved, we
work around this limitation by generating the redundant argument and kind
information anyway. Thankfully, this is simple to accomplish, as we already
compute this information to begin with.

Note [Generating type families with the correct arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As of GHC 9.10, GHC no longer performs arity inference when kind-checking type
family declarations with standalone kind signatures. This is an important
consideration when promoting functions with top-level type signatures. For
example, we would not want to take this definition:

  f :: Either a Bool
  f = Right True

And promote it to this type family:

  type F :: Either a Bool
  type family F where
    F = Right True

GHC would reject this type family because it would expect F to have arity 0,
but its definition requires arity 1. This is because the definition of F is
tantamount to writing:

  F @a = Right @a @Bool True -- This takes 1 argument, hence arity 1

In order to make F kind-check, we need to generate a type family header that
explicitly declares it to have arity 1, not arity 0:

  type F :: Either a Bool
  type family F @a where
    F = Right True

Note the @a binder after F in the type family header.

If the standalone kind signature lacks an outermost forall, then we simply bind
the type variables in left-to-right order, preserving dependencies (using
`toposortTyVarsOf`). If the standalone kind signature does have an outermost
`forall`, then we bind the type variables according to the order in which it
appears in the `forall`, making sure to filter out any inferred type variable
binders. For example, we would want to take this definition (from #585):

  konst :: forall a {b}. a -> b -> a
  konst x _ = x

And promote it to this type family:

  type Konst :: forall a {b}. a -> b -> a
  type family Konst @a x y where
    Konst @a (x :: a) (_ :: b) = x

Note that we do not bind @b here. The `tvbSpecsToBndrVis` function is
responsible for filtering out inferred type variable binders.
-}

promoteClause :: Maybe Uniq
                 -- ^ Let-binding unique (if locally bound)
              -> Name
                 -- ^ Name of the function being promoted
              -> Maybe LetDecRHSKindInfo
                 -- ^ Information about the promoted kind (if present)
              -> [Name]
                 -- ^ The local variables currently in scope
              -> DClause -> PrM (DTySynEqn, ADClause)
promoteClause mb_let_uniq name m_ldrki all_locals (DClause pats exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  ((types, pats'), prom_pat_infos) <- evalForPair $ mapAndUnzipM promotePat pats
  -- If the function has scoped type variables, then we annotate each argument
  -- in the promoted type family equation with its kind.
  -- See Note [Scoped type variables] in Data.Singletons.TH.Promote.Monad for an
  -- explanation of why we do this.
  let types_w_kinds =
        case m_ldrki of
          Just (LDRKI _ scoped_tvs _ kinds _)
            |  not (OSet.null scoped_tvs)
            -> zipWith DSigT types kinds
          _ -> types
  let PromDPatInfos { prom_dpat_vars    = new_vars
                    , prom_dpat_sig_kvs = sig_kvs } = prom_pat_infos
  (ty, ann_exp) <- scopedBind sig_kvs $
                   lambdaBind new_vars $
                   promoteExp exp
  pro_clause_fun <- promoteLetDecName mb_let_uniq name m_ldrki all_locals
  return ( DTySynEqn Nothing (foldType pro_clause_fun types_w_kinds) ty
         , ADClause new_vars pats' ann_exp )

promoteMatch :: DType -- What to use as the LHS of the promoted type family
                      -- equation. This should consist of the promoted name of
                      -- the case expression to which the match belongs, applied
                      -- to any local arguments (e.g., `Case x y z`).
             -> DMatch -> PrM (DTySynEqn, ADMatch)
promoteMatch pro_case_fun (DMatch pat exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  ((ty, pat'), prom_pat_infos) <- evalForPair $ promotePat pat
  let PromDPatInfos { prom_dpat_vars    = new_vars
                    , prom_dpat_sig_kvs = sig_kvs } = prom_pat_infos
  (rhs, ann_exp) <- scopedBind sig_kvs $
                    lambdaBind new_vars $
                    promoteExp exp
  return $ ( DTySynEqn Nothing (pro_case_fun `DAppT` ty) rhs
           , ADMatch new_vars pat' ann_exp)

-- promotes a term pattern into a type pattern, accumulating bound variable names
promotePat :: DPat -> QWithAux PromDPatInfos PrM (DType, ADPat)
promotePat (DLitP lit) = (, ADLitP lit) <$> promoteLitPat lit
promotePat (DVarP name) = do
      -- term vars can be symbols... type vars can't!
  tyName <- mkTyName name
  tell $ PromDPatInfos [(name, tyName)] OSet.empty
  return (DVarT tyName, ADVarP name)
promotePat (DConP name tys pats) = do
  opts <- getOptions
  kis <- traverse (promoteType_options conOptions) tys
  (types, pats') <- mapAndUnzipM promotePat pats
  let name' = promotedDataTypeOrConName opts name
  return (foldType (foldl DAppKindT (DConT name') kis) types, ADConP name kis pats')
  where
    -- Currently, visible type patterns of data constructors are the one place
    -- in `singletons-th` where it makes sense to promote wildcard types, as it
    -- will produce code that GHC will accept.
    conOptions :: PromoteTypeOptions
    conOptions = defaultPromoteTypeOptions{ptoAllowWildcards = True}
promotePat (DTildeP pat) = do
  qReportWarning "Lazy pattern converted into regular pattern in promotion"
  second ADTildeP <$> promotePat pat
promotePat (DBangP pat) = do
  qReportWarning "Strict pattern converted into regular pattern in promotion"
  second ADBangP <$> promotePat pat
promotePat (DSigP pat ty) = do
  -- We must maintain the invariant that any promoted pattern signature must
  -- not have any wildcards in the underlying pattern.
  -- See Note [Singling pattern signatures].
  wildless_pat <- removeWilds pat
  (promoted, pat') <- promotePat wildless_pat
  ki <- promoteType ty
  tell $ PromDPatInfos [] (fvDType ki)
  return (DSigT promoted ki, ADSigP promoted pat' ki)
promotePat DWildP = return (DWildCardT, ADWildP)
promotePat p@(DTypeP _) = fail ("Embedded type patterns cannot be promoted: " ++ show p)
promotePat p@(DInvisP _) = fail ("Invisible type patterns cannot be promoted: " ++ show p)

promoteExp :: DExp -> PrM (DType, ADExp)
promoteExp (DVarE name) = fmap (, ADVarE name) $ lookupVarE name
promoteExp (DConE name) = do
  opts <- getOptions
  return (DConT $ defunctionalizedName0 opts name, ADConE name)
promoteExp (DLitE lit)  = fmap (, ADLitE lit) $ promoteLitExp lit
promoteExp (DAppE exp1 exp2) = do
  (exp1', ann_exp1) <- promoteExp exp1
  (exp2', ann_exp2) <- promoteExp exp2
  return (apply exp1' exp2', ADAppE ann_exp1 ann_exp2)
-- Until we get visible kind applications, this is the best we can do.
promoteExp (DAppTypeE exp _) = do
  qReportWarning "Visible type applications are ignored by `singletons-th`."
  promoteExp exp
promoteExp (DLamE names exp) = do
  opts <- getOptions
  lambdaName <- newUniqueName "Lambda"
  tyNames <- mapM mkTyName names
  let var_proms = zip names tyNames
  (rhs, ann_exp) <- lambdaBind var_proms $ promoteExp exp
  all_locals <- allLocals
  let tvbs     = map (`DPlainTV` BndrReq) tyNames
      all_args = all_locals ++ tyNames
      tfh      = dTypeFamilyHead_with_locals lambdaName all_locals tvbs DNoSig
  emitDecs [DClosedTypeFamilyD
              tfh
              [DTySynEqn Nothing
                         (foldType (DConT lambdaName) (map DVarT all_args))
                         rhs]]
  emitDecsM $ defunctionalize lambdaName Nothing $ DefunNoSAK all_locals tvbs Nothing
  let promLambda = foldType (DConT (defunctionalizedName opts lambdaName 0))
                            (map DVarT all_locals)
  return (promLambda, ADLamE tyNames promLambda names ann_exp)
promoteExp (DCaseE exp matches) = do
  caseTFName <- newUniqueName "Case"
  all_locals <- allLocals
  let prom_case = foldType (DConT caseTFName) (map DVarT all_locals)
  (exp', ann_exp)     <- promoteExp exp
  (eqns, ann_matches) <- mapAndUnzipM (promoteMatch prom_case) matches
  tyvarName  <- qNewName "t"
  let tvbs = [DPlainTV tyvarName BndrReq]
      tfh  = dTypeFamilyHead_with_locals caseTFName all_locals tvbs DNoSig
  emitDecs [DClosedTypeFamilyD tfh eqns]
    -- See Note [Annotate case return type] in Single
  let applied_case = prom_case `DAppT` exp'
  return ( applied_case
         , ADCaseE ann_exp ann_matches applied_case )
promoteExp (DLetE decs exp) = do
  unique <- qNewUnique
  (binds, ann_env) <- promoteLetDecs (Just unique) decs
  (exp', ann_exp) <- letBind binds $ promoteExp exp
  return (exp', ADLetE ann_env ann_exp)
promoteExp (DSigE exp ty) = do
  (exp', ann_exp) <- promoteExp exp
  ty' <- promoteType ty
  return (DSigT exp' ty', ADSigE exp' ann_exp ty')
promoteExp e@(DStaticE _) = fail ("Static expressions cannot be promoted: " ++ show e)
promoteExp e@(DTypedBracketE _) = fail ("Typed bracket expressions cannot be promoted: " ++ show e)
promoteExp e@(DTypedSpliceE _) = fail ("Typed splice expressions cannot be promoted: " ++ show e)
promoteExp e@(DTypeE _) = fail ("Embedded type expressions cannot be promoted: " ++ show e)

promoteLitExp :: OptionsMonad q => Lit -> q DType
promoteLitExp (IntegerL n) = do
  opts <- getOptions
  let tyFromIntegerName = promotedValueName opts fromIntegerName Nothing
      tyNegateName      = promotedValueName opts negateName      Nothing
  if n >= 0
     then return $ (DConT tyFromIntegerName `DAppT` DLitT (NumTyLit n))
     else return $ (DConT tyNegateName `DAppT`
                    (DConT tyFromIntegerName `DAppT` DLitT (NumTyLit (-n))))
promoteLitExp (StringL str) = do
  opts <- getOptions
  let prom_str_lit = DLitT (StrTyLit str)
  os_enabled <- qIsExtEnabled LangExt.OverloadedStrings
  pure $ if os_enabled
         then DConT (promotedValueName opts fromStringName Nothing) `DAppT` prom_str_lit
         else prom_str_lit
promoteLitExp (CharL c) = return $ DLitT (CharTyLit c)
promoteLitExp lit =
  fail ("Only string, natural number, and character literals can be promoted: " ++ show lit)

promoteLitPat :: MonadFail m => Lit -> m DType
promoteLitPat (IntegerL n)
  | n >= 0    = return $ (DLitT (NumTyLit n))
  | otherwise =
    fail $ "Negative literal patterns are not allowed,\n" ++
           "because literal patterns are promoted to natural numbers."
promoteLitPat (StringL str) = return $ DLitT (StrTyLit str)
promoteLitPat (CharL c) = return $ DLitT (CharTyLit c)
promoteLitPat lit =
  fail ("Only string, natural number, and character literals can be promoted: " ++ show lit)

-- Promote the name of a 'ULetDecRHS' to the type level. If the promoted
-- 'ULetDecRHS' has a standalone type signature and does not close over any
-- local variables, then this will include the scoped type variables from the
-- type signature as invisible arguments. (See Note [Scoped type variables] in
-- Data.Singletons.TH.Promote.Monad.) Otherwise, it will include any local
-- variables that it closes over as explicit arguments.
promoteLetDecName ::
     Maybe Uniq
     -- ^ Let-binding unique (if locally bound)
  -> Name
     -- ^ Name of the function being promoted
  -> Maybe LetDecRHSKindInfo
     -- ^ Information about the promoted kind (if present)
  -> [Name]
     -- ^ The local variables currently in scope
  -> PrM DType
promoteLetDecName mb_let_uniq name m_ldrki all_locals = do
  opts <- getOptions
  let proName = promotedValueName opts name mb_let_uniq
      type_args =
        case m_ldrki of
          Just (LDRKI m_sak _ tvbs _ _)
            |  isJust m_sak
               -- Per the comments on LetDecRHSKindInfo, `isJust m_sak` is only True
               -- if there are no local variables. Convert the scoped type variables
               -- `tvbs` to invisible arguments, making sure to use
               -- `tvbSpecsToBndrVis` to filter out any inferred type variable
               -- binders. For instance, we want to promote this example (from #585):
               --
               --   konst :: forall a {b}. a -> b -> a
               --   konst x _ = x
               --
               -- To this type family:
               --
               --   type Konst :: forall a {b}. a -> b -> a
               --   type family Konst @a x y where
               --     Konst @a (x :: a) (_ :: b) = x
               --
               -- Note that we apply `a` in `Konst @a` but _not_ `b`, as `b` is
               -- bound using an inferred type variable binder.
            -> map dTyVarBndrVisToDTypeArg $ tvbSpecsToBndrVis tvbs
          _ -> -- ...otherwise, return the local variables as explicit arguments
               -- using DTANormal.
               map (DTANormal . DVarT) all_locals
  pure $ applyDType (DConT proName) type_args

-- Construct a 'DTypeFamilyHead' that closes over some local variables. We
-- apply `noExactName` to each local variable to avoid GHC#11812.
-- See also Note [Pitfalls of NameU/NameL] in Data.Singletons.TH.Util.
dTypeFamilyHead_with_locals ::
     Name
  -- ^ Name of type family
  -> [Name]
  -- ^ Local variables
  -> [DTyVarBndrVis]
  -- ^ Variables for type family arguments
  -> DFamilyResultSig
  -- ^ Type family result
  -> DTypeFamilyHead
dTypeFamilyHead_with_locals tf_nm local_nms arg_tvbs res_sig =
  DTypeFamilyHead
    tf_nm
    (map (`DPlainTV` BndrReq) local_nms' ++ arg_tvbs')
    res_sig'
    Nothing
  where
    -- We take care to only apply `noExactName` to the local variables and not
    -- to any of the argument/result types. The latter are much more likely to
    -- show up in the Haddocks, and `noExactName` produces incredibly long Names
    -- that are much harder to read in the rendered Haddocks.
    local_nms' = map noExactName local_nms

    -- Ensure that all references to local_nms are substituted away.
    subst1 = Map.fromList $
             zipWith (\local_nm local_nm' -> (local_nm, DVarT local_nm'))
                     local_nms
                     local_nms'
    (subst2, arg_tvbs') = substTvbs subst1 arg_tvbs
    (_subst3, res_sig') = substFamilyResultSig subst2 res_sig
