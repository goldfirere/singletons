{- Data/Singletons/Single.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains functions to refine constructs to work with singleton
types. It is an internal module to the singletons package.
-}
{-# LANGUAGE TemplateHaskell, TupleSections, ParallelListComp, CPP #-}

module Data.Singletons.Single where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (Quasi(..))
import Data.Singletons.Deriving.Infer
import Data.Singletons.Deriving.Ord
import Data.Singletons.Deriving.Bounded
import Data.Singletons.Deriving.Enum
import Data.Singletons.Deriving.Show
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons.Promote.Monad ( promoteM )
import Data.Singletons.Promote.Type
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Type
import Data.Singletons.Single.Data
import Data.Singletons.Single.Fixity
import Data.Singletons.Single.Eq
import Data.Singletons.Syntax
import Data.Singletons.Partition
import Language.Haskell.TH.Desugar
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import Data.Maybe
import Control.Monad
import Data.List
import Data.Singletons.Internal (mangleDeclsWithTypes)
import qualified GHC.LanguageExtensions.Type as LangExt

{-
How singletons works
~~~~~~~~~~~~~~~~~~~~

Singling, on the surface, doesn't seem all that complicated. Promote the type,
and singletonize all the terms. That's essentially what was done singletons < 1.0.
But, now we want to deal with higher-order singletons. So, things are a little
more complicated.

The way to understand all of this is that *every* variable maps to something
of type (Sing t), for an appropriately-kinded t. This includes functions, which
use the "SLambda" instance of Sing. To apply singleton functions, we use the
applySing function.

That, in and of itself, wouldn't be too hard, but it's really annoying from
the user standpoint. After dutifully singling `map`, a user doesn't want to
have to use two `applySing`s to actually use it. So, any let-bound identifier
is eta-expanded so that the singled type has the same number of arrows as
the original type. (If there is no original type signature, then it has as
many arrows as the original had patterns.) Then, we store a use of one of the
singFunX functions in the SgM environment so that every use of a let-bound
identifier has a proper type (Sing t).

It would be consistent to avoid this eta-expansion for local lets (as opposed
to top-level lets), but that seemed like more bother than it was worth. It
may also be possible to be cleverer about nested eta-expansions and contractions,
but that also seemed not to be worth it. Though I haven't tested it, my hope
is that the eta-expansions and contractions have no runtime effect, especially
because SLambda is a *newtype* instance, not a *data* instance.

Note that to maintain the desired invariant, we must also be careful to eta-
contract constructors. This is the point of buildDataLets.
-}

-- | Generate singleton definitions from a type that is already defined.
-- For example, the singletons package itself uses
--
-- > $(genSingletons [''Bool, ''Maybe, ''Either, ''[]])
--
-- to generate singletons for Prelude types.
genSingletons :: DsMonad q => [Name] -> q [Dec]
genSingletons names = do
  checkForRep names
  ddecs <- concatMapM (singInfo <=< dsInfo <=< reifyWithWarning) names
  return $ decsToTH ddecs

-- | Make promoted and singleton versions of all declarations given, retaining
-- the original declarations.
-- See <https://github.com/goldfirere/singletons/blob/master/README.md> for
-- further explanation.
singletons :: DsMonad q => q [Dec] -> q [Dec]
singletons qdecs = do
  decs <- qdecs
  singDecs <- wrapDesugar singTopLevelDecs decs
  return (decs ++ singDecs)

-- | Make promoted and singleton versions of all declarations given, discarding
-- the original declarations. Note that a singleton based on a datatype needs
-- the original datatype, so this will fail if it sees any datatype declarations.
-- Classes, instances, and functions are all fine.
singletonsOnly :: DsMonad q => q [Dec] -> q [Dec]
singletonsOnly = (>>= wrapDesugar singTopLevelDecs)

-- | Create instances of 'SEq' and type-level @(==)@ for each type in the list
singEqInstances :: DsMonad q => [Name] -> q [Dec]
singEqInstances = concatMapM singEqInstance

-- | Create instance of 'SEq' and type-level @(==)@ for the given type
singEqInstance :: DsMonad q => Name -> q [Dec]
singEqInstance name = do
  promotion <- promoteEqInstance name
  dec <- singEqualityInstance sEqClassDesc name
  return $ dec ++ promotion

-- | Create instances of 'SEq' (only -- no instance for @(==)@, which 'SEq' generally
-- relies on) for each type in the list
singEqInstancesOnly :: DsMonad q => [Name] -> q [Dec]
singEqInstancesOnly = concatMapM singEqInstanceOnly

-- | Create instances of 'SEq' (only -- no instance for @(==)@, which 'SEq' generally
-- relies on) for the given type
singEqInstanceOnly :: DsMonad q => Name -> q [Dec]
singEqInstanceOnly name = singEqualityInstance sEqClassDesc name

-- | Create instances of 'SDecide' for each type in the list.
singDecideInstances :: DsMonad q => [Name] -> q [Dec]
singDecideInstances = concatMapM singDecideInstance

-- | Create instance of 'SDecide' for the given type.
singDecideInstance :: DsMonad q => Name -> q [Dec]
singDecideInstance name = singEqualityInstance sDecideClassDesc name

-- generalized function for creating equality instances
singEqualityInstance :: DsMonad q => EqualityClassDesc q -> Name -> q [Dec]
singEqualityInstance desc@(_, _, className, _) name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of " ++
                            show className ++ " for it.") name
  dtvbs <- mapM dsTvb tvbs
  dcons <- concatMapM dsCon cons
  let tyvars = map (DVarT . extractTvbName) dtvbs
      kind = foldType (DConT name) tyvars
  (scons, _) <- singM [] $ mapM singCtor dcons
  eqInstance <- mkEqualityInstance Nothing kind scons desc
  return $ decToTH eqInstance

-- | Create instances of 'SOrd' for the given types
singOrdInstances :: DsMonad q => [Name] -> q [Dec]
singOrdInstances = concatMapM singOrdInstance

-- | Create instance of 'SOrd' for the given type
singOrdInstance :: DsMonad q => Name -> q [Dec]
singOrdInstance = singInstance mkOrdInstance "Ord"

-- | Create instances of 'SBounded' for the given types
singBoundedInstances :: DsMonad q => [Name] -> q [Dec]
singBoundedInstances = concatMapM singBoundedInstance

-- | Create instance of 'SBounded' for the given type
singBoundedInstance :: DsMonad q => Name -> q [Dec]
singBoundedInstance = singInstance mkBoundedInstance "Bounded"

-- | Create instances of 'SEnum' for the given types
singEnumInstances :: DsMonad q => [Name] -> q [Dec]
singEnumInstances = concatMapM singEnumInstance

-- | Create instance of 'SEnum' for the given type
singEnumInstance :: DsMonad q => Name -> q [Dec]
singEnumInstance = singInstance mkEnumInstance "Enum"

-- | Create instance of 'SShow' for the given type
--
-- (Not to be confused with 'showShowInstance'.)
singShowInstance :: DsMonad q => Name -> q [Dec]
singShowInstance = singInstance (mkShowInstance ForPromotion) "Show"

-- | Create instances of 'SShow' for the given types
--
-- (Not to be confused with 'showSingInstances'.)
singShowInstances :: DsMonad q => [Name] -> q [Dec]
singShowInstances = concatMapM singShowInstance

-- | Create instance of 'ShowSing' for the given type
--
-- (Not to be confused with 'singShowInstance'.)

-- (We can't simply use singInstance to create ShowSing instances, because
-- there's no promoted counterpart. So we use this instead.)
showSingInstance :: DsMonad q => Name -> q [Dec]
showSingInstance name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of ShowSing for it.") name
  dtvbs <- mapM dsTvb tvbs
  dcons <- concatMapM dsCon cons
  let tyvars = map (DVarT . extractTvbName) dtvbs
      kind = foldType (DConT name) tyvars
      deriv_show_decl = DerivedDecl { ded_mb_cxt = Nothing
                                    , ded_type   = kind
                                    , ded_cons   = dcons }
  (show_insts, _) <- singM [] $ singDerivedShowDecs deriv_show_decl
  pure $ decsToTH show_insts

-- | Create instances of 'ShowSing' for the given types
--
-- (Not to be confused with 'singShowInstances'.)
showSingInstances :: DsMonad q => [Name] -> q [Dec]
showSingInstances = concatMapM showSingInstance

singInstance :: DsMonad q
             => (Maybe DCxt -> DType -> [DCon] -> q UInstDecl)
             -> String -> Name -> q [Dec]
singInstance mk_inst inst_name name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of " ++ inst_name
                            ++ " for it.") name
  dtvbs <- mapM dsTvb tvbs
  dcons <- concatMapM dsCon cons
  raw_inst <- mk_inst Nothing (foldType (DConT name) (map tvbToType dtvbs)) dcons
  (a_inst, decs) <- promoteM [] $
                    promoteInstanceDec Map.empty raw_inst
  decs' <- singDecsM [] $ (:[]) <$> singInstD a_inst
  return $ decsToTH (decs ++ decs')

singInfo :: DsMonad q => DInfo -> q [DDec]
singInfo (DTyConI dec _) =
  singTopLevelDecs [] [dec]
singInfo (DPrimTyConI _name _numArgs _unlifted) =
  fail "Singling of primitive type constructors not supported"
singInfo (DVarI _name _ty _mdec) =
  fail "Singling of value info not supported"
singInfo (DTyVarI _name _ty) =
  fail "Singling of type variable info not supported"
singInfo (DPatSynI {}) =
  fail "Singling of pattern synonym info not supported"

singTopLevelDecs :: DsMonad q => [Dec] -> [DDec] -> q [DDec]
singTopLevelDecs locals raw_decls = withLocalDeclarations locals $ do
  let raw_decls' = mangleDeclsWithTypes  raw_decls
  decls <- expand raw_decls'     -- expand type synonyms
  PDecs { pd_let_decs          = letDecls
        , pd_class_decs        = classes
        , pd_instance_decs     = insts
        , pd_data_decs         = datas
        , pd_derived_eq_decs   = derivedEqDecs
        , pd_derived_show_decs = derivedShowDecs } <- partitionDecs decls

  ((letDecEnv, classes', insts'), promDecls) <- promoteM locals $ do

    promoteDataDecs datas
    (_, letDecEnv) <- promoteLetDecs noPrefix letDecls
    classes' <- mapM promoteClassDec classes
    let meth_sigs = foldMap (lde_types . cd_lde) classes
    insts' <- mapM (promoteInstanceDec meth_sigs) insts
    mapM_ promoteDerivedEqDec derivedEqDecs
    return (letDecEnv, classes', insts')

  singDecsM locals $ do
    let letBinds = concatMap buildDataLets datas
                ++ concatMap buildMethLets classes
    (newLetDecls, newDecls) <- bindLets letBinds $
                               singLetDecEnv letDecEnv $ do
                                 newDataDecls <- concatMapM singDataD datas
                                 newClassDecls <- mapM singClassD classes'
                                 newInstDecls <- mapM singInstD insts'
                                 newDerivedEqDecs <- concatMapM singDerivedEqDecs derivedEqDecs
                                 newDerivedShowDecs <- concatMapM singDerivedShowDecs derivedShowDecs
                                 return $ newDataDecls ++ newClassDecls
                                                       ++ newInstDecls
                                                       ++ newDerivedEqDecs
                                                       ++ newDerivedShowDecs
    return $ promDecls ++ (map DLetDec newLetDecls) ++ newDecls

-- rewriteDeclInnerFn  :: DDec -> DDec
-- rewriteDeclInnerFn d@(DDataDecl _ _ _ _ _) -> rewriteInnerFunction d

-- rewriteInnerFunction' :: DDec -> DDec
-- rewriteInnerFunction' (DDataD _nd ctx data_name nvps cons _derivings) =
--     DDataD _nd ctx data_name nvps (catMaybes cons') _derivings
--     where
--         cons' :: [Maybe DCon]
--         cons' = flip map cons $ \d@(DCon _ _ _ _ c) -> case c of
--            Just (DAppT (DAppT DArrowT _) _) -> Nothing
--            _                      -> Just d
-- rewriteInnerFunction' x = x


-- rewriteInnerFunction'' :: Dec -> Dec
-- rewriteInnerFunction'' (DataD _nd ctx data_name nvps cons _derivings) =
--     DataD _nd ctx data_name nvps (catMaybes cons') _derivings
--     where
--         -- cons' :: 
--         -- cons' = flip map cons $ \case
--             -- NormalC nm ts -> NormalC 
--         cons' = flip map cons $ \d@(Con _ _ _ _ c) -> case c of
--            Just (AppT (AppT ArrowT _) _) -> Nothing
--            _                      -> Just d
-- rewriteInnerFunction'' x = x

-- see comment at top of file
buildDataLets :: DataDecl -> [(Name, DExp)]
buildDataLets (DataDecl _nd _name _tvbs cons _derivings) =
  concatMap con_num_args cons
  where
    con_num_args :: DCon -> [(Name, DExp)]
    con_num_args (DCon _tvbs _cxt name fields _rty) =
      (name, wrapSingFun (length (tysOfConFields fields))
                         (promoteValRhs name) (DConE $ singDataConName name))
      : rec_selectors fields

    rec_selectors :: DConFields -> [(Name, DExp)]
    rec_selectors (DNormalC {}) = []
    rec_selectors (DRecC fields) =
      let names = map fstOf3 fields in
      [ (name, wrapSingFun 1 (promoteValRhs name) (DVarE $ singValName name))
      | name <- names ]

-- see comment at top of file
buildMethLets :: UClassDecl -> [(Name, DExp)]
buildMethLets (ClassDecl { cd_lde = LetDecEnv { lde_types = meth_sigs } }) =
  map mk_bind (Map.toList meth_sigs)
  where
    mk_bind (meth_name, meth_ty) =
      ( meth_name
      , wrapSingFun (countArgs meth_ty) (promoteValRhs meth_name)
                                        (DVarE $ singValName meth_name) )

singClassD :: AClassDecl -> SgM DDec
singClassD (ClassDecl { cd_cxt  = cls_cxt
                      , cd_name = cls_name
                      , cd_tvbs = cls_tvbs
                      , cd_fds  = cls_fundeps
                      , cd_lde  = LetDecEnv { lde_defns = default_defns
                                            , lde_types = meth_sigs
                                            , lde_infix = fixities
                                            , lde_proms = promoted_defaults } }) = do
  (sing_sigs, _, tyvar_names, res_kis)
    <- unzip4 <$> zipWithM (singTySig no_meth_defns meth_sigs)
                           meth_names (map promoteValRhs meth_names)
  let default_sigs = catMaybes $ zipWith3 mk_default_sig meth_names sing_sigs res_kis
      res_ki_map   = Map.fromList (zip meth_names
                                       (map (fromMaybe always_sig) res_kis))
  sing_meths <- mapM (uncurry (singLetDecRHS (Map.fromList tyvar_names)
                                             res_ki_map))
                     (Map.toList default_defns)
  fixities' <- traverse (uncurry singInfixDecl) fixities
  cls_cxt' <- mapM singPred cls_cxt
  return $ DClassD cls_cxt'
                   (singClassName cls_name)
                   cls_tvbs
                   cls_fundeps   -- they are fine without modification
                   (map DLetDec (sing_sigs ++ sing_meths ++ fixities') ++ default_sigs)
  where
    no_meth_defns = error "Internal error: can't find declared method type"
    always_sig    = error "Internal error: no signature for default method"
    meth_names    = Map.keys meth_sigs

    mk_default_sig meth_name (DSigD s_name sty) (Just res_ki) =
      DDefaultSigD s_name <$> add_constraints meth_name sty res_ki
    mk_default_sig _ _ _ = error "Internal error: a singled signature isn't a signature."

    add_constraints meth_name sty res_ki = do  -- Maybe monad
      prom_dflt <- Map.lookup meth_name promoted_defaults
      let default_pred = foldl DAppPr (DConPr equalityName)
                                -- NB: Need the res_ki here to prevent ambiguous
                                -- kinds in result-inferred default methods.
                                -- See #175
                               [ foldApply (promoteValRhs meth_name) tvs `DSigT` res_ki
                               , foldApply prom_dflt tvs ]
      return $ DForallT tvbs (default_pred : cxt) (ravel args res)
      where
        (tvbs, cxt, args, res) = unravel sty
        tvs                    = map tvbToType tvbs


singInstD :: AInstDecl -> SgM DDec
singInstD (InstDecl { id_cxt = cxt, id_name = inst_name
                    , id_arg_tys = inst_tys, id_meths = ann_meths }) = do
  cxt' <- mapM singPred cxt
  inst_kis <- mapM promoteType inst_tys
  meths <- concatMapM (uncurry sing_meth) ann_meths
  return (DInstanceD Nothing
                     cxt'
                     (foldl DAppT (DConT s_inst_name) inst_kis)
                     meths)

  where
    s_inst_name = singClassName inst_name

    sing_meth :: Name -> ALetDecRHS -> SgM [DDec]
    sing_meth name rhs = do
      mb_s_info <- dsReify (singValName name)
      (s_ty, tyvar_names, m_res_ki) <- case mb_s_info of
        Just (DVarI _ (DForallT cls_tvbs _cls_pred s_ty) _) -> do
          let (sing_tvbs, _pred, _args, res_ty) = unravel s_ty
          inst_kis <- mapM promoteType inst_tys
          let subst = Map.fromList (zip (map extractTvbName cls_tvbs)
                                        inst_kis)
              m_res_ki = case res_ty of
                _sing `DAppT` (_prom_func `DSigT` res_ki) -> Just (substKind subst res_ki)
                _                                         -> Nothing

          return (substType subst s_ty, map extractTvbName sing_tvbs, m_res_ki)
        _ -> do
          mb_info <- dsReify name
          case mb_info of
            Just (DVarI _ (DForallT cls_tvbs _cls_pred inner_ty) _) -> do
              let subst = Map.fromList (zip (map extractTvbName cls_tvbs)
                                            inst_tys)
              -- Make sure to expand through type synonyms here! Not doing so
              -- resulted in #167.
              raw_ty <- expand inner_ty
              (s_ty, _num_args, tyvar_names, res_ki) <- singType (promoteValRhs name)
                                                                 (substType subst raw_ty)
              return (s_ty, tyvar_names, Just res_ki)
            _ -> fail $ "Cannot find type of method " ++ show name

      let kind_map = maybe Map.empty (Map.singleton name) m_res_ki
      meth' <- singLetDecRHS (Map.singleton name tyvar_names)
                             kind_map name rhs
      return $ map DLetDec [DSigD (singValName name) s_ty, meth']

singLetDecEnv :: ALetDecEnv -> SgM a -> SgM ([DLetDec], a)
singLetDecEnv (LetDecEnv { lde_defns = defns
                         , lde_types = types
                         , lde_infix = infix_decls
                         , lde_proms = proms })
              thing_inside = do
  let prom_list = Map.toList proms
  (typeSigs, letBinds, tyvarNames, res_kis)
    <- unzip4 <$> mapM (uncurry (singTySig defns types)) prom_list
  infix_decls' <- traverse (uncurry singInfixDecl) infix_decls
  let res_ki_map = Map.fromList [ (name, res_ki) | ((name, _), Just res_ki)
                                                     <- zip prom_list res_kis ]
  bindLets letBinds $ do
    let_decs <- mapM (uncurry (singLetDecRHS (Map.fromList tyvarNames) res_ki_map))
                     (Map.toList defns)
    thing <- thing_inside
    return (infix_decls' ++ typeSigs ++ let_decs, thing)

singTySig :: Map Name ALetDecRHS  -- definitions
          -> Map Name DType       -- type signatures
          -> Name -> DType   -- the type is the promoted type, not the type sig!
          -> SgM ( DLetDec               -- the new type signature
                 , (Name, DExp)          -- the let-bind entry
                 , (Name, [Name])        -- the scoped tyvar names in the tysig
                 , Maybe DKind           -- the result kind in the tysig
                 )
singTySig defns types name prom_ty =
  let sName = singValName name in
  case Map.lookup name types of
    Nothing -> do
      num_args <- guess_num_args
      (sty, tyvar_names) <- mk_sing_ty num_args
      return ( DSigD sName sty
             , (name, wrapSingFun num_args prom_ty (DVarE sName))
             , (name, tyvar_names)
             , Nothing )
    Just ty -> do
      (sty, num_args, tyvar_names, res_ki) <- singType prom_ty ty
      return ( DSigD sName sty
             , (name, wrapSingFun num_args prom_ty (DVarE sName))
             , (name, tyvar_names)
             , Just res_ki )
  where
    guess_num_args :: SgM Int
    guess_num_args =
      case Map.lookup name defns of
        Nothing -> fail "Internal error: promotion known for something not let-bound."
        Just (AValue _ n _) -> return n
        Just (AFunction _ n _) -> return n

      -- create a Sing t1 -> Sing t2 -> ... type of a given arity and result type
    mk_sing_ty :: Int -> SgM (DType, [Name])
    mk_sing_ty n = do
      arg_names <- replicateM n (qNewName "arg")
      return ( DForallT (map DPlainTV arg_names) []
                        (ravel (map (\nm -> singFamily `DAppT` DVarT nm) arg_names)
                               (singFamily `DAppT`
                                    (foldl apply prom_ty (map DVarT arg_names))))
             , arg_names )

singLetDecRHS :: Map Name [Name]
              -> Map Name DKind   -- result kind (might not be known)
              -> Name -> ALetDecRHS -> SgM DLetDec
singLetDecRHS _bound_names res_kis name (AValue prom num_arrows exp) =
  DValD (DVarPa (singValName name)) <$>
  (wrapUnSingFun num_arrows prom <$> singExp exp (Map.lookup name res_kis))
singLetDecRHS bound_names res_kis name (AFunction prom_fun num_arrows clauses) =
  let tyvar_names = case Map.lookup name bound_names of
                      Nothing -> []
                      Just ns -> ns
      res_ki = Map.lookup name res_kis
  in
  DFunD (singValName name) <$>
        mapM (singClause prom_fun num_arrows tyvar_names res_ki) clauses

singClause :: DType   -- the promoted function
           -> Int     -- the number of arrows in the type. If this is more
                      -- than the number of patterns, we need to eta-expand
                      -- with unSingFun.
           -> [Name]  -- the names of the forall'd vars in the type sig of this
                      -- function. This list should have at least the length as the
                      -- number of patterns in the clause
           -> Maybe DKind   -- result kind, if known
           -> ADClause -> SgM DClause
singClause prom_fun num_arrows bound_names res_ki
           (ADClause var_proms pats exp) = do

  -- Fix #166:
  when (num_arrows - length pats < 0) $
    fail $ "Function being promoted to " ++ (pprint (typeToTH prom_fun)) ++
           " has too many arguments."

  sPats <- mapM (singPat (Map.fromList var_proms)) pats
  sBody <- singExp exp res_ki
    -- when calling unSingFun, the promoted pats aren't in scope, so we use the
    -- bound_names instead
  let pattern_bound_names = zipWith const bound_names pats
       -- this does eta-expansion. See comment at top of file.
      sBody' = wrapUnSingFun (num_arrows - length pats)
                 (foldl apply prom_fun (map DVarT pattern_bound_names)) sBody
  return $ DClause sPats sBody'

singPat :: Map Name Name   -- from term-level names to type-level names
        -> DPat
        -> SgM DPat
singPat _var_proms (DLitPa _lit) =
  fail "Singling of literal patterns not yet supported"
singPat var_proms (DVarPa name) = do
  tyname <- case Map.lookup name var_proms of
              Nothing     ->
                fail "Internal error: unknown variable when singling pattern"
              Just tyname -> return tyname
  return $ DVarPa (singValName name) `DSigPa` (singFamily `DAppT` DVarT tyname)
singPat var_proms (DConPa name pats) = do
  pats' <- mapM (singPat var_proms) pats
  return $ DConPa (singDataConName name) pats'
singPat var_proms (DTildePa pat) = do
  qReportWarning
    "Lazy pattern converted into regular pattern during singleton generation."
  singPat var_proms pat
singPat var_proms (DBangPa pat) = do
  pat' <- singPat var_proms pat
  return $ DBangPa pat'
singPat _var_proms (DSigPa _pat _ty) = error "TODO: Handle SigPa. See Issue #183."
singPat _var_proms DWildPa = return DWildPa


-- Note [Annotate case return type]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We're straining GHC's type inference here. One particular trouble area
-- is determining the return type of a GADT pattern match. In general, GHC
-- cannot infer return types of GADT pattern matches because the return type
-- becomes "untouchable" in the case matches. See the OutsideIn paper. But,
-- during singletonization, we *know* the return type. So, just add a type
-- annotation. See #54.

-- Note [Why error is so special]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Some of the transformations that happen before this point produce impossible
-- case matches. We must be careful when processing these so as not to make
-- an error GHC will complain about. When binding the case-match variables, we
-- normally include an equality constraint saying that the scrutinee is equal
-- to the matched pattern. But, we can't do this in inaccessible matches, because
-- equality is bogus, and GHC (rightly) complains. However, we then have another
-- problem, because GHC doesn't have enough information when type-checking the
-- RHS of the inaccessible match to deem it type-safe. The solution: treat error
-- as super-special, so that GHC doesn't look too hard at singletonized error
-- calls. Specifically, DON'T do the applySing stuff. Just use sError, which
-- has a custom type (Sing x -> a) anyway.

singExp :: ADExp -> Maybe DKind   -- the kind of the expression, if known
        -> SgM DExp
  -- See Note [Why error is so special]
singExp (ADVarE err `ADAppE` arg) _res_ki
  | err == errorName = DAppE (DVarE (singValName err)) <$>
                       singExp arg (Just (DConT symbolName))
singExp (ADVarE name) _res_ki = lookupVarE name
singExp (ADConE name) _res_ki = lookupConE name
singExp (ADLitE lit)  _res_ki = singLit lit
singExp (ADAppE e1 e2) _res_ki = do
  e1' <- singExp e1 Nothing
  e2' <- singExp e2 Nothing
  -- `applySing undefined x` kills type inference, because GHC can't figure
  -- out the type of `undefined`. So we don't emit `applySing` there.
  if isException e1'
  then return $ e1' `DAppE` e2'
  else return $ (DVarE applySingName) `DAppE` e1' `DAppE` e2'
singExp (ADLamE ty_names prom_lam names exp) _res_ki = do
  let sNames = map singValName names
  exp' <- singExp exp Nothing
  -- we need to bind the type variables... but DLamE doesn't allow SigT patterns.
  -- So: build a case
  let caseExp = DCaseE (mkTupleDExp (map DVarE sNames))
                       [DMatch (mkTupleDPat
                                (map ((DWildPa `DSigPa`) .
                                      (singFamily `DAppT`) .
                                      DVarT) ty_names)) exp']
  return $ wrapSingFun (length names) prom_lam $ DLamE sNames caseExp
singExp (ADCaseE exp matches ret_ty) res_ki =
    -- See Note [Annotate case return type]
  DSigE <$> (DCaseE <$> singExp exp Nothing <*> mapM (singMatch res_ki) matches)
        <*> pure (singFamily `DAppT` (ret_ty `maybeSigT` res_ki))
singExp (ADLetE env exp) res_ki =
  uncurry DLetE <$> singLetDecEnv env (singExp exp res_ki)
singExp (ADSigE {}) _ =
  fail "Singling of explicit type annotations not yet supported."

-- See Note [DerivedDecl]
singDerivedEqDecs :: DerivedEqDecl -> SgM [DDec]
singDerivedEqDecs (DerivedDecl { ded_mb_cxt = mb_ctxt
                               , ded_type   = ty
                               , ded_cons   = cons }) = do
  (scons, _) <- singM [] $ mapM singCtor cons
  mb_sctxt <- mapM (mapM singPred) mb_ctxt
  kind <- promoteType ty
  sEqInst <- mkEqualityInstance mb_sctxt kind scons sEqClassDesc
  -- Beware! The user might have specified an instance context like this:
  --
  --   deriving instance Eq a => Eq (T a Int)
  --
  -- When we single the context, it will become (SEq a). But we do *not* want
  -- this for the SDecide instance! The simplest solution is to simply replace
  -- all occurrences of SEq with SDecide in the context.
  let mb_sctxtDecide = fmap (map sEqToSDecide) mb_sctxt
  sDecideInst <- mkEqualityInstance mb_sctxtDecide kind scons sDecideClassDesc
  return [sEqInst, sDecideInst]

-- Walk a DPred, replacing all occurrences of SEq with SDecide.
sEqToSDecide :: DPred -> DPred
sEqToSDecide = modifyConNameDPred $ \n ->
  -- Why don't we directly compare n to sEqClassName? Because n is almost certainly
  -- produced from a call to singClassName, which uses unqualified Names. Ugh.
  if nameBase n == nameBase sEqClassName
     then sDecideClassName
     else n

-- See Note [DerivedDecl]
singDerivedShowDecs :: DerivedShowDecl -> SgM [DDec]
singDerivedShowDecs (DerivedDecl { ded_mb_cxt = mb_cxt
                                 , ded_type   = ty
                                 , ded_cons   = cons }) = do
    -- First, generate the ShowSing instance.
    show_sing_inst <- mkShowInstance ForShowSing mb_cxt ty cons
    z <- qNewName "z"
    -- Next, the Show instance for the singleton type, like this:
    --
    --   instance (ShowSing a, ShowSing b) => Sing (Sing (z :: Either a b)) where
    --     showsPrec = showsSingPrec
    --
    -- Be careful: we want to generate an instance context that uses ShowSing,
    -- not Show, because we are reusing the ShowSing instance.
    let show_cxt  = inferConstraintsDef (fmap (mkShowContext ForShowSing) mb_cxt)
                                        (DConPr showSingName)
                                        cons
        show_inst = DInstanceD Nothing show_cxt
                               (DConT showName `DAppT` (singFamily `DAppT` DSigT (DVarT z) ty))
                               [DLetDec (DFunD showsPrecName
                                               [DClause [] (DVarE showsSingPrecName)])]
    pure [toInstanceD show_sing_inst, show_inst]
  where
    toInstanceD :: UInstDecl -> DDec
    toInstanceD (InstDecl { id_cxt = cxt, id_name = inst_name
                     , id_arg_tys = inst_tys, id_meths = ann_meths }) =
      DInstanceD Nothing cxt (foldType (DConT inst_name) inst_tys)
                         (map (DLetDec . toFunD) ann_meths)

    toFunD :: (Name, ULetDecRHS) -> DLetDec
    toFunD (fun_name, UFunction clauses) = DFunD fun_name clauses
    toFunD (val_name, UValue rhs)        = DValD (DVarPa val_name) rhs

isException :: DExp -> Bool
isException (DVarE n)             = nameBase n == "sUndefined"
isException (DConE {})            = False
isException (DLitE {})            = False
isException (DAppE (DVarE fun) _) | nameBase fun == "sError" = True
isException (DAppE fun _)         = isException fun
isException (DAppTypeE e _)       = isException e
isException (DLamE _ _)           = False
isException (DCaseE e _)          = isException e
isException (DLetE _ e)           = isException e
isException (DSigE e _)           = isException e
isException (DStaticE e)          = isException e

singMatch :: Maybe DKind  -- ^ the result kind, if known
          -> ADMatch -> SgM DMatch
singMatch res_ki (ADMatch var_proms pat exp) = do
  sPat <- singPat (Map.fromList var_proms) pat
  sExp <- singExp exp res_ki
  return $ DMatch sPat sExp

singLit :: Lit -> SgM DExp
singLit (IntegerL n)
  | n >= 0    = return $
                DVarE sFromIntegerName `DAppE`
                (DVarE singMethName `DSigE`
                 (singFamily `DAppT` DLitT (NumTyLit n)))
  | otherwise = do sLit <- singLit (IntegerL (-n))
                   return $ DVarE sNegateName `DAppE` sLit
singLit (StringL str) = do
  let sing_str_lit = DVarE singMethName `DSigE`
                     (singFamily `DAppT` DLitT (StrTyLit str))
  os_enabled <- qIsExtEnabled LangExt.OverloadedStrings
  pure $ if os_enabled
         then DVarE sFromStringName `DAppE` sing_str_lit
         else sing_str_lit
singLit lit =
  fail ("Only string and natural number literals can be singled: " ++ show lit)

maybeSigT :: DType -> Maybe DKind -> DType
maybeSigT ty Nothing   = ty
maybeSigT ty (Just ki) = ty `DSigT` ki
