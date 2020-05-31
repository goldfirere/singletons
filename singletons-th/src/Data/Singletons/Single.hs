{- Data/Singletons/Single.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains functions to refine constructs to work with singleton
types. It is an internal module to the singletons-th package.
-}
{-# LANGUAGE TemplateHaskellQuotes, TupleSections, ParallelListComp #-}

module Data.Singletons.Single where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (NameSpace(..), Quasi(..))
import Data.Singletons.Deriving.Eq
import Data.Singletons.Deriving.Ord
import Data.Singletons.Deriving.Bounded
import Data.Singletons.Deriving.Enum
import Data.Singletons.Deriving.Show
import Data.Singletons.Deriving.Util
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons.Promote.Defun
import Data.Singletons.Promote.Monad ( promoteM )
import Data.Singletons.Promote.Type
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Type
import Data.Singletons.Single.Data
import Data.Singletons.Single.Decide
import Data.Singletons.Single.Defun
import Data.Singletons.Single.Fixity
import Data.Singletons.Syntax
import Data.Singletons.TH.Options
import Data.Singletons.Partition
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OMap.Strict as OMap
import Language.Haskell.TH.Desugar.OMap.Strict (OMap)
import qualified Language.Haskell.TH.Desugar.OSet as OSet
import Language.Haskell.TH.Desugar.OSet (OSet)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import Data.Maybe
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Trans.Class
import Data.List (unzip6, zipWith4)
import qualified GHC.LanguageExtensions.Type as LangExt

{-
How singletons-th works
~~~~~~~~~~~~~~~~~~~~~~~

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

-- | Generate singled definitions for each of the provided type-level
-- declaration 'Name's. For example, the singletons-th package itself uses
--
-- > $(genSingletons [''Bool, ''Maybe, ''Either, ''[]])
--
-- to generate singletons for Prelude types.
genSingletons :: OptionsMonad q => [Name] -> q [Dec]
genSingletons names = do
  opts <- getOptions
  -- See Note [Disable genQuotedDecs in genPromotions and genSingletons]
  -- in D.S.Promote
  withOptions opts{genQuotedDecs = False} $ do
    checkForRep names
    ddecs <- concatMapM (singInfo <=< dsInfo <=< reifyWithLocals) names
    return $ decsToTH ddecs

-- | Make promoted and singled versions of all declarations given, retaining
-- the original declarations. See the
-- @<https://github.com/goldfirere/singletons/blob/master/README.md README>@
-- for further explanation.
singletons :: OptionsMonad q => q [Dec] -> q [Dec]
singletons qdecs = do
  opts <- getOptions
  withOptions opts{genQuotedDecs = True} $ singletons' $ lift qdecs

-- | Make promoted and singled versions of all declarations given, discarding
-- the original declarations. Note that a singleton based on a datatype needs
-- the original datatype, so this will fail if it sees any datatype declarations.
-- Classes, instances, and functions are all fine.
singletonsOnly :: OptionsMonad q => q [Dec] -> q [Dec]
singletonsOnly qdecs = do
  opts <- getOptions
  withOptions opts{genQuotedDecs = False} $ singletons' $ lift qdecs

-- The workhorse for 'singletons' and 'singletonsOnly'. The difference between
-- the two functions is whether 'genQuotedDecs' is set to 'True' or 'False'.
singletons' :: OptionsMonad q => q [Dec] -> q [Dec]
singletons' qdecs = do
  opts     <- getOptions
  decs     <- qdecs
  ddecs    <- withLocalDeclarations decs $ dsDecs decs
  singDecs <- singTopLevelDecs decs ddecs
  let origDecs | genQuotedDecs opts = decs
               | otherwise          = []
  return $ origDecs ++ decsToTH singDecs

-- | Create instances of 'SEq' for the given types
singEqInstances :: OptionsMonad q => [Name] -> q [Dec]
singEqInstances = concatMapM singEqInstance

-- | Create instance of 'SEq' for the given type
singEqInstance :: OptionsMonad q => Name -> q [Dec]
singEqInstance = singInstance mkEqInstance "Eq"

-- | Create instances of 'SDecide', 'TestEquality', and 'TestCoercion' for each
-- type in the list.
singDecideInstances :: OptionsMonad q => [Name] -> q [Dec]
singDecideInstances = concatMapM singDecideInstance

-- | Create instances of 'SDecide', 'TestEquality', and 'TestCoercion' for the
-- given type.
singDecideInstance :: OptionsMonad q => Name -> q [Dec]
singDecideInstance name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of SDecide for it.") name
  dtvbs <- mapM dsTvb tvbs
  let data_ty = foldTypeTvbs (DConT name) dtvbs
  dcons <- concatMapM (dsCon dtvbs data_ty) cons
  let tyvars = map (DVarT . extractTvbName) dtvbs
      kind = foldType (DConT name) tyvars
  (scons, _) <- singM [] $ mapM (singCtor name) dcons
  sDecideInstance <- mkDecideInstance Nothing kind dcons scons
  testInstances <- traverse (mkTestInstance Nothing kind name dcons)
                            [TestEquality, TestCoercion]
  return $ decsToTH (sDecideInstance:testInstances)

-- | Create instances of 'SOrd' for the given types
singOrdInstances :: OptionsMonad q => [Name] -> q [Dec]
singOrdInstances = concatMapM singOrdInstance

-- | Create instance of 'SOrd' for the given type
singOrdInstance :: OptionsMonad q => Name -> q [Dec]
singOrdInstance = singInstance mkOrdInstance "Ord"

-- | Create instances of 'SBounded' for the given types
singBoundedInstances :: OptionsMonad q => [Name] -> q [Dec]
singBoundedInstances = concatMapM singBoundedInstance

-- | Create instance of 'SBounded' for the given type
singBoundedInstance :: OptionsMonad q => Name -> q [Dec]
singBoundedInstance = singInstance mkBoundedInstance "Bounded"

-- | Create instances of 'SEnum' for the given types
singEnumInstances :: OptionsMonad q => [Name] -> q [Dec]
singEnumInstances = concatMapM singEnumInstance

-- | Create instance of 'SEnum' for the given type
singEnumInstance :: OptionsMonad q => Name -> q [Dec]
singEnumInstance = singInstance mkEnumInstance "Enum"

-- | Create instance of 'SShow' for the given type
--
-- (Not to be confused with 'showShowInstance'.)
singShowInstance :: OptionsMonad q => Name -> q [Dec]
singShowInstance = singInstance (mkShowInstance ForPromotion) "Show"

-- | Create instances of 'SShow' for the given types
--
-- (Not to be confused with 'showSingInstances'.)
singShowInstances :: OptionsMonad q => [Name] -> q [Dec]
singShowInstances = concatMapM singShowInstance

-- | Create instance of 'Show' for the given singleton type
--
-- (Not to be confused with 'singShowInstance'.)
showSingInstance :: OptionsMonad q => Name -> q [Dec]
showSingInstance name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of Show for it.") name
  dtvbs <- mapM dsTvb tvbs
  let data_ty = foldTypeTvbs (DConT name) dtvbs
  dcons <- concatMapM (dsCon dtvbs data_ty) cons
  let tyvars    = map (DVarT . extractTvbName) dtvbs
      kind      = foldType (DConT name) tyvars
      data_decl = DataDecl name dtvbs dcons
      deriv_show_decl = DerivedDecl { ded_mb_cxt     = Nothing
                                    , ded_type       = kind
                                    , ded_type_tycon = name
                                    , ded_decl       = data_decl }
  (show_insts, _) <- singM [] $ singDerivedShowDecs deriv_show_decl
  pure $ decsToTH show_insts

-- | Create instances of 'Show' for the given singleton types
--
-- (Not to be confused with 'singShowInstances'.)
showSingInstances :: OptionsMonad q => [Name] -> q [Dec]
showSingInstances = concatMapM showSingInstance

-- | Create an instance for @'SingI' TyCon{N}@, where @N@ is the positive
-- number provided as an argument.
--
-- Note that the generated code requires the use of the @QuantifiedConstraints@
-- language extension.
singITyConInstances :: DsMonad q => [Int] -> q [Dec]
singITyConInstances = concatMapM singITyConInstance

-- | Create an instance for @'SingI' TyCon{N}@, where @N@ is the positive
-- number provided as an argument.
--
-- Note that the generated code requires the use of the @QuantifiedConstraints@
-- language extension.
singITyConInstance :: DsMonad q => Int -> q [Dec]
singITyConInstance n
  | n <= 0
  = fail $ "Argument must be a positive number (given " ++ show n ++ ")"
  | otherwise
  = do as <- replicateM n (qNewName "a")
       ks <- replicateM n (qNewName "k")
       k_last <- qNewName "k_last"
       f      <- qNewName "f"
       x      <- qNewName "x"
       let k_penult = last ks
           k_fun = ravelVanillaDType [] [] (map DVarT ks) (DVarT k_last)
           f_ty  = DVarT f
           a_tys = map DVarT as
           mk_fun arrow t1 t2 = arrow `DAppT` t1 `DAppT` t2
           matchable_apply_fun   = mk_fun DArrowT                (DVarT k_penult) (DVarT k_last)
           unmatchable_apply_fun = mk_fun (DConT tyFunArrowName) (DVarT k_penult) (DVarT k_last)
           ctxt = [ DForallT ForallInvis (map DPlainTV as) $
                    DConstrainedT (map (DAppT (DConT singIName)) a_tys)
                                  (DConT singIName `DAppT` foldType f_ty a_tys)
                  , DConT equalityName
                      `DAppT` (DConT applyTyConName `DSigT`
                                mk_fun DArrowT matchable_apply_fun unmatchable_apply_fun)
                      `DAppT` DConT applyTyConAux1Name
                  ]
       pure $ decToTH
            $ DInstanceD
                Nothing Nothing ctxt
                (DConT singIName `DAppT` (DConT (mkTyConName n) `DAppT` (f_ty `DSigT` k_fun)))
                [DLetDec $ DFunD singMethName
                           [DClause [] $
                            wrapSingFun 1 DWildCardT $
                            DLamE [x] $
                            DVarE withSingIName `DAppE` DVarE x
                                                `DAppE` DVarE singMethName]]

singInstance :: OptionsMonad q => DerivDesc q -> String -> Name -> q [Dec]
singInstance mk_inst inst_name name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of " ++ inst_name
                            ++ " for it.") name
  dtvbs <- mapM dsTvb tvbs
  let data_ty = foldTypeTvbs (DConT name) dtvbs
  dcons <- concatMapM (dsCon dtvbs data_ty) cons
  let data_decl = DataDecl name dtvbs dcons
  raw_inst <- mk_inst Nothing data_ty data_decl
  (a_inst, decs) <- promoteM [] $
                    promoteInstanceDec OMap.empty Map.empty raw_inst
  decs' <- singDecsM [] $ (:[]) <$> singInstD a_inst
  return $ decsToTH (decs ++ decs')

singInfo :: OptionsMonad q => DInfo -> q [DDec]
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

singTopLevelDecs :: OptionsMonad q => [Dec] -> [DDec] -> q [DDec]
singTopLevelDecs locals raw_decls = withLocalDeclarations locals $ do
  decls <- expand raw_decls     -- expand type synonyms
  PDecs { pd_let_decs                = letDecls
        , pd_class_decs              = classes
        , pd_instance_decs           = insts
        , pd_data_decs               = datas
        , pd_ty_syn_decs             = ty_syns
        , pd_open_type_family_decs   = o_tyfams
        , pd_closed_type_family_decs = c_tyfams
        , pd_derived_eq_decs         = derivedEqDecs
        , pd_derived_show_decs       = derivedShowDecs } <- partitionDecs decls

  ((letDecEnv, classes', insts'), promDecls) <- promoteM locals $ do
    defunTopLevelTypeDecls ty_syns c_tyfams o_tyfams
    recSelLetDecls <- promoteDataDecs datas
    (_, letDecEnv) <- promoteLetDecs Nothing $ recSelLetDecls ++ letDecls
    classes' <- mapM promoteClassDec classes
    let meth_sigs    = foldMap (lde_types . cd_lde) classes
        cls_tvbs_map = Map.fromList $ map (\cd -> (cd_name cd, cd_tvbs cd)) classes
    insts' <- mapM (promoteInstanceDec meth_sigs cls_tvbs_map) insts
    return (letDecEnv, classes', insts')

  singDecsM locals $ do
    dataLetBinds <- concatMapM buildDataLets datas
    methLetBinds <- concatMapM buildMethLets classes
    let letBinds = dataLetBinds ++ methLetBinds
    (newLetDecls, singIDefunDecls, newDecls)
                            <- bindLets letBinds $
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
    return $ promDecls ++ (map DLetDec newLetDecls) ++ singIDefunDecls ++ newDecls

-- see comment at top of file
buildDataLets :: OptionsMonad q => DataDecl -> q [(Name, DExp)]
buildDataLets (DataDecl _name _tvbs cons) = do
  opts <- getOptions
  pure $ concatMap (con_num_args opts) cons
  where
    con_num_args :: Options -> DCon -> [(Name, DExp)]
    con_num_args opts (DCon _tvbs _cxt name fields _rty) =
      (name, wrapSingFun (length (tysOfConFields fields))
                         (DConT $ defunctionalizedName0 opts name)
                         (DConE $ singledDataConName opts name))
      : rec_selectors opts fields

    rec_selectors :: Options -> DConFields -> [(Name, DExp)]
    rec_selectors _    (DNormalC {}) = []
    rec_selectors opts (DRecC fields) =
      let names = map fstOf3 fields in
      [ (name, wrapSingFun 1 (DConT $ defunctionalizedName0 opts name)
                             (DVarE $ singledValueName opts name))
      | name <- names ]

-- see comment at top of file
buildMethLets :: OptionsMonad q => UClassDecl -> q [(Name, DExp)]
buildMethLets (ClassDecl { cd_lde = LetDecEnv { lde_types = meth_sigs } }) = do
  opts <- getOptions
  pure $ map (mk_bind opts) (OMap.assocs meth_sigs)
  where
    mk_bind opts (meth_name, meth_ty) =
      ( meth_name
      , wrapSingFun (countArgs meth_ty) (DConT $ defunctionalizedName0 opts meth_name)
                                        (DVarE $ singledValueName opts meth_name) )

singClassD :: AClassDecl -> SgM DDec
singClassD (ClassDecl { cd_cxt  = cls_cxt
                      , cd_name = cls_name
                      , cd_tvbs = cls_tvbs
                      , cd_fds  = cls_fundeps
                      , cd_lde  = LetDecEnv { lde_defns     = default_defns
                                            , lde_types     = meth_sigs
                                            , lde_infix     = fixities
                                            , lde_proms     = promoted_defaults
                                            , lde_bound_kvs = meth_bound_kvs } }) =
  bindContext [foldTypeTvbs (DConT cls_name) cls_tvbs] $ do
    opts <- getOptions
    mb_cls_sak <- dsReifyType cls_name
    let sing_cls_name   = singledClassName opts cls_name
        mb_sing_cls_sak = fmap (DKiSigD sing_cls_name) mb_cls_sak
    cls_infix_decls <- singReifiedInfixDecls $ cls_name:meth_names
    (sing_sigs, _, tyvar_names, cxts, res_kis, singIDefunss)
      <- unzip6 <$> zipWithM (singTySig no_meth_defns meth_sigs meth_bound_kvs)
                             meth_names
                             (map (DConT . defunctionalizedName0 opts) meth_names)
    emitDecs $ maybeToList mb_sing_cls_sak ++ cls_infix_decls ++ concat singIDefunss
    let default_sigs = catMaybes $
                       zipWith4 (mk_default_sig opts) meth_names sing_sigs
                                                      tyvar_names res_kis
        res_ki_map   = Map.fromList (zip meth_names
                                         (map (fromMaybe always_sig) res_kis))
    sing_meths <- mapM (uncurry (singLetDecRHS (Map.fromList tyvar_names)
                                               (Map.fromList cxts)
                                               res_ki_map))
                       (OMap.assocs default_defns)
    fixities' <- mapMaybeM (uncurry singInfixDecl) $ OMap.assocs fixities
    cls_cxt' <- mapM singPred cls_cxt
    return $ DClassD cls_cxt'
                     sing_cls_name
                     cls_tvbs
                     cls_fundeps   -- they are fine without modification
                     (map DLetDec (sing_sigs ++ sing_meths ++ fixities') ++ default_sigs)
  where
    no_meth_defns = error "Internal error: can't find declared method type"
    always_sig    = error "Internal error: no signature for default method"
    meth_names    = map fst $ OMap.assocs meth_sigs

    mk_default_sig opts meth_name (DSigD s_name sty) bound_kvs (Just res_ki) =
      DDefaultSigD s_name <$> add_constraints opts meth_name sty bound_kvs res_ki
    mk_default_sig _ _ _ _ _ = error "Internal error: a singled signature isn't a signature."

    add_constraints opts meth_name sty (_, bound_kvs) res_ki = do  -- Maybe monad
      (tvbs, cxt, args, res) <- unravelVanillaDType sty
      prom_dflt <- OMap.lookup meth_name promoted_defaults

      -- Filter out explicitly bound kind variables. Otherwise, if you had
      -- the following class (#312):
      --
      --  class Foo a where
      --    bar :: a -> b -> b
      --    bar _ x = x
      --
      -- Then it would be singled to:
      --
      --  class SFoo a where
      --    sBar :: forall b (x :: a) (y :: b). Sing x -> Sing y -> Sing (sBar x y)
      --    default :: forall b (x :: a) (y :: b).
      --               (Bar b x y) ~ (BarDefault b x y) => ...
      --
      -- Which applies Bar/BarDefault to b, which shouldn't happen.
      let tvs = map tvbToType $
                filter (\tvb -> extractTvbName tvb `Set.member` bound_kv_set) tvbs
          prom_meth =  DConT $ defunctionalizedName0 opts meth_name
          default_pred = foldType (DConT equalityName)
                                -- NB: Need the res_ki here to prevent ambiguous
                                -- kinds in result-inferred default methods.
                                -- See #175
                               [ foldApply prom_meth tvs `DSigT` res_ki
                               , foldApply prom_dflt tvs ]
      return $ ravelVanillaDType tvbs (default_pred : cxt) args res
      where
        bound_kv_set = Set.fromList bound_kvs

singInstD :: AInstDecl -> SgM DDec
singInstD (InstDecl { id_cxt = cxt, id_name = inst_name, id_arg_tys = inst_tys
                    , id_sigs = inst_sigs, id_meths = ann_meths }) = do
  opts <- getOptions
  let s_inst_name = singledClassName opts inst_name
  bindContext cxt $ do
    cxt' <- mapM singPred cxt
    inst_kis <- mapM promoteType inst_tys
    meths <- concatMapM (uncurry sing_meth) ann_meths
    return (DInstanceD Nothing
                       Nothing
                       cxt'
                       (foldl DAppT (DConT s_inst_name) inst_kis)
                       meths)

  where
    sing_meth :: Name -> ALetDecRHS -> SgM [DDec]
    sing_meth name rhs = do
      opts <- getOptions
      mb_s_info <- dsReify (singledValueName opts name)
      inst_kis <- mapM promoteType inst_tys
      let mk_subst cls_tvbs = Map.fromList $ zip (map extractTvbName vis_cls_tvbs) inst_kis
            where
              -- This is a half-hearted attempt to address the underlying problem
              -- in #358, where we can sometimes have more class type variables
              -- (due to implicit kind arguments) than class arguments. This just
              -- ensures that the explicit type variables are properly mapped
              -- to the class arguments, leaving the implicit kind variables
              -- unmapped. That could potentially cause *other* problems, but
              -- those are perhaps best avoided by using InstanceSigs. At the
              -- very least, this workaround will make error messages slightly
              -- less confusing.
              vis_cls_tvbs = drop (length cls_tvbs - length inst_kis) cls_tvbs

          sing_meth_ty :: OSet Name -> DType
                       -> SgM (DType, [Name], DCxt, DKind)
          sing_meth_ty bound_kvs inner_ty = do
            -- Make sure to expand through type synonyms here! Not doing so
            -- resulted in #167.
            raw_ty <- expand inner_ty
            (s_ty, _num_args, tyvar_names, ctxt, _arg_kis, res_ki)
              <- singType bound_kvs (DConT $ defunctionalizedName0 opts name) raw_ty
            pure (s_ty, tyvar_names, ctxt, res_ki)

      (s_ty, tyvar_names, ctxt, m_res_ki) <- case OMap.lookup name inst_sigs of
        Just inst_sig -> do
          -- We have an InstanceSig, so just single that type. Take care to
          -- avoid binding the variables bound by the instance head as well.
          let inst_bound = foldMap fvDType (cxt ++ inst_kis)
          (s_ty, tyvar_names, ctxt, res_ki) <- sing_meth_ty inst_bound inst_sig
          pure (s_ty, tyvar_names, ctxt, Just res_ki)
        Nothing -> case mb_s_info of
          -- We don't have an InstanceSig, so we must compute the type to use
          -- in the singled instance ourselves through reification.
          Just (DVarI _ (DForallT _ cls_tvbs (DConstrainedT _cls_pred s_ty)) _) -> do
            (sing_tvbs, ctxt, _args, res_ty) <- unravelVanillaDType s_ty
            let subst = mk_subst cls_tvbs
                m_res_ki = case res_ty of
                  _sing `DAppT` (_prom_func `DSigT` res_ki) -> Just (substKind subst res_ki)
                  _                                         -> Nothing

            pure ( substType subst s_ty
                 , map extractTvbName sing_tvbs
                 , map (substType subst) ctxt
                 , m_res_ki )
          _ -> do
            mb_info <- dsReify name
            case mb_info of
              Just (DVarI _ (DForallT _ cls_tvbs
                                      (DConstrainedT _cls_pred inner_ty)) _) -> do
                let subst = mk_subst cls_tvbs
                    cls_kvb_names = foldMap (foldMap fvDType . extractTvbKind) cls_tvbs
                    cls_tvb_names = OSet.fromList $ map extractTvbName cls_tvbs
                    cls_bound     = cls_kvb_names `OSet.union` cls_tvb_names
                (s_ty, tyvar_names, ctxt, res_ki) <- sing_meth_ty cls_bound inner_ty
                pure ( substType subst s_ty
                     , tyvar_names
                     , ctxt
                     , Just (substKind subst res_ki) )
              _ -> fail $ "Cannot find type of method " ++ show name

      let kind_map = maybe Map.empty (Map.singleton name) m_res_ki
      meth' <- singLetDecRHS (Map.singleton name tyvar_names)
                             (Map.singleton name ctxt)
                             kind_map name rhs
      return $ map DLetDec [DSigD (singledValueName opts name) s_ty, meth']

singLetDecEnv :: ALetDecEnv
              -> SgM a
              -> SgM ([DLetDec], [DDec], a)
                 -- Return:
                 --
                 -- 1. The singled let-decs
                 -- 2. SingI instances for any defunctionalization symbols
                 --    (see Data.Singletons.Single.Defun)
                 -- 3. The result of running the `SgM a` action
singLetDecEnv (LetDecEnv { lde_defns     = defns
                         , lde_types     = types
                         , lde_infix     = infix_decls
                         , lde_proms     = proms
                         , lde_bound_kvs = bound_kvs })
              thing_inside = do
  let prom_list = OMap.assocs proms
  (typeSigs, letBinds, tyvarNames, cxts, res_kis, singIDefunss)
    <- unzip6 <$> mapM (uncurry (singTySig defns types bound_kvs)) prom_list
  infix_decls' <- mapMaybeM (uncurry singInfixDecl) $ OMap.assocs infix_decls
  let res_ki_map = Map.fromList [ (name, res_ki) | ((name, _), Just res_ki)
                                                     <- zip prom_list res_kis ]
  bindLets letBinds $ do
    let_decs <- mapM (uncurry (singLetDecRHS (Map.fromList tyvarNames)
                                             (Map.fromList cxts)
                                             res_ki_map))
                     (OMap.assocs defns)
    thing <- thing_inside
    return (infix_decls' ++ typeSigs ++ let_decs, concat singIDefunss, thing)

singTySig :: OMap Name ALetDecRHS  -- definitions
          -> OMap Name DType       -- type signatures
          -> OMap Name (OSet Name) -- bound kind variables
          -> Name -> DType   -- the type is the promoted type, not the type sig!
          -> SgM ( DLetDec               -- the new type signature
                 , (Name, DExp)          -- the let-bind entry
                 , (Name, [Name])        -- the scoped tyvar names in the tysig
                 , (Name, DCxt)          -- the context of the type signature
                 , Maybe DKind           -- the result kind in the tysig
                 , [DDec]                -- SingI instances for defun symbols
                 )
singTySig defns types bound_kvs name prom_ty = do
  opts <- getOptions
  let sName = singledValueName opts name
  case OMap.lookup name types of
    Nothing -> do
      num_args <- guess_num_args
      (sty, tyvar_names) <- mk_sing_ty num_args
      singIDefuns <- singDefuns name VarName []
                                (map (const Nothing) tyvar_names) Nothing
      return ( DSigD sName sty
             , (name, wrapSingFun num_args prom_ty (DVarE sName))
             , (name, tyvar_names)
             , (name, [])
             , Nothing
             , singIDefuns )
    Just ty -> do
      all_bound_kvs <- lookup_bound_kvs
      (sty, num_args, tyvar_names, ctxt, arg_kis, res_ki)
        <- singType all_bound_kvs prom_ty ty
      bound_cxt <- askContext
      singIDefuns <- singDefuns name VarName (bound_cxt ++ ctxt)
                                (map Just arg_kis) (Just res_ki)
      return ( DSigD sName sty
             , (name, wrapSingFun num_args prom_ty (DVarE sName))
             , (name, tyvar_names)
             , (name, ctxt)
             , Just res_ki
             , singIDefuns )
  where
    guess_num_args :: SgM Int
    guess_num_args =
      case OMap.lookup name defns of
        Nothing -> fail "Internal error: promotion known for something not let-bound."
        Just (AValue _ n _) -> return n
        Just (AFunction _ n _) -> return n

    lookup_bound_kvs :: SgM (OSet Name)
    lookup_bound_kvs =
      case OMap.lookup name bound_kvs of
        Nothing -> fail $ "Internal error: " ++ nameBase name ++ " has no type variable "
                          ++ "bindings, despite having a type signature"
        Just kvs -> pure kvs

      -- create a Sing t1 -> Sing t2 -> ... type of a given arity and result type
    mk_sing_ty :: Int -> SgM (DType, [Name])
    mk_sing_ty n = do
      arg_names <- replicateM n (qNewName "arg")
      -- If there are no arguments, use `Sing @_` instead of `Sing`.
      -- See Note [Disable kind generalization for local functions if possible]
      let sing_w_wildcard | n == 0    = singFamily `DAppKindT` DWildCardT
                          | otherwise = singFamily
      return ( ravelVanillaDType
                 (map DPlainTV arg_names)
                 []
                 (map (\nm -> singFamily `DAppT` DVarT nm) arg_names)
                 (sing_w_wildcard `DAppT`
                      (foldl apply prom_ty (map DVarT arg_names)))
             , arg_names )

{-
Note [Disable kind generalization for local functions if possible]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example (from #296):

  f :: forall a. MyProxy a -> MyProxy a
  f MyProxy =
    let x = let z :: MyProxy a
                z = MyProxy in z
    in x

A naïve attempt at singling `f` is as follows:

  type LetZ :: MyProxy a
  type family LetZ where
    LetZ = 'MyProxy

  type family LetX where
    LetX = LetZ

  type F :: forall a. MyProxy a -> MyProxy a
  type family F x where
    F 'MyProxy = LetX

  sF :: forall a (t :: MyProxy a). Sing t -> Sing (F t :: MyProxy a)
  sF SMyProxy =
    let sX :: Sing LetX
        sX = let sZ :: Sing (LetZ :: MyProxy a)
                 sZ = SMyProxy in sZ
    in sX

This will not typecheck, however. The is because the return kind of
`LetX` (in `let sX :: Sing LetX`) will get generalized by virtue of `sX`
having a type signature. It's as if one had written this:

  sF :: forall a (t :: MyProxy a). Sing t -> Sing (F t :: MyProxy a)
  sF SMyProxy =
    let sX :: forall a1. Sing (LetX :: MyProxy a1)
        sX = ...

This is too general, since `sX` will only typecheck if the return kind of
`LetX` is `MyProxy a`, not `MyProxy a1`. In order to avoid this problem,
we need to avoid kind generalization when kind-checking the type of `sX`.
To accomplish this, we borrow a trick from
Note [The id hack; or, how singletons-th learned to stop worrying and avoid kind generalization]
and use TypeApplications plus a wildcard type. That is, we generate this code
for `sF`:

  sF :: forall a (t :: MyProxy a). Sing t -> Sing (F t :: MyProxy a)
  sF SMyProxy =
    let sX :: Sing @_ LetX
        sX = ...

The presence of the wildcard type disables kind generalization, which allows
GHC's kind inference to deduce that the return kind of `LetX` should be `a`.
Now `sF` typechecks, and since we only use wildcards within visible kind
applications, we don't even have to force users to enable
PartialTypeSignatures. Hooray!

Question: where should we put wildcard types when singling? One possible answer
is: put a wildcard in any type signature that gets generated when singling a
function that lacks a type signature. Unfortunately, this is a step too far.
This will break singling the `foldr` function:

    foldr                   :: (a -> b -> b) -> b -> [a] -> b
    foldr k z = go
              where
                go []     = z
                go (y:ys) = y `k` go ys

If the type of `sGo` is given a wildcard, then it will fail to typecheck. This
is because `sGo` is polymorphically recursive, so disabling kind generalization
forces GHC to infer `sGo`'s type. Attempting to infer a polymorphically
recursive type, unsurprisingly, leads to failure.

To avoid this sort of situation, where adopt a simple metric: if a function
lacks a type signature, only put @_ in its singled type signature if it has
zero arguments. This allows `sX` to typecheck without breaking things like
`sGo`. This metric is a bit conservative, however, since it means that this
small tweak to `x` still would not typecheck:

  f :: forall a. MyProxy a -> MyProxy a
  f MyProxy =
    let x () = let z :: MyProxy a
                   z = MyProxy in z
    in x ()

We need not let perfect be the enemy of good, however. It is extremely
common for local definitions to have zero arguments, so it makes good sense
to optimize for that special case. In fact, this special treatment is the only
reason that `foo8` from the `T183` test case singles successfully, since
the as-patterns in `foo8` desugar to code very similar to the `f` example
above.
-}

singLetDecRHS :: Map Name [Name]
              -> Map Name DCxt    -- the context of the type signature
                                  -- (might not be known)
              -> Map Name DKind   -- result kind (might not be known)
              -> Name -> ALetDecRHS -> SgM DLetDec
singLetDecRHS bound_names cxts res_kis name ld_rhs = do
  opts <- getOptions
  bindContext (Map.findWithDefault [] name cxts) $
    case ld_rhs of
      AValue prom num_arrows exp ->
        DValD (DVarP (singledValueName opts name)) <$>
        (wrapUnSingFun num_arrows prom <$> singExp exp (Map.lookup name res_kis))
      AFunction prom_fun num_arrows clauses ->
        let tyvar_names = case Map.lookup name bound_names of
                            Nothing -> []
                            Just ns -> ns
            res_ki = Map.lookup name res_kis
        in
        DFunD (singledValueName opts name) <$>
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

  (sPats, sigPaExpsSigs) <- evalForPair $ mapM (singPat (Map.fromList var_proms)) pats
  sBody <- singExp exp res_ki
    -- when calling unSingFun, the promoted pats aren't in scope, so we use the
    -- bound_names instead
  let pattern_bound_names = zipWith const bound_names pats
       -- this does eta-expansion. See comment at top of file.
      sBody' = wrapUnSingFun (num_arrows - length pats)
                 (foldl apply prom_fun (map DVarT pattern_bound_names)) sBody
  return $ DClause sPats $ mkSigPaCaseE sigPaExpsSigs sBody'

singPat :: Map Name Name   -- from term-level names to type-level names
        -> ADPat
        -> QWithAux SingDSigPaInfos SgM DPat
singPat var_proms = go
  where
    go :: ADPat -> QWithAux SingDSigPaInfos SgM DPat
    go (ADLitP _lit) =
      fail "Singling of literal patterns not yet supported"
    go (ADVarP name) = do
      opts <- getOptions
      tyname <- case Map.lookup name var_proms of
                  Nothing     ->
                    fail "Internal error: unknown variable when singling pattern"
                  Just tyname -> return tyname
      pure $ DVarP (singledValueName opts name)
               `DSigP` (singFamily `DAppT` DVarT tyname)
    go (ADConP name pats) = do
      opts <- getOptions
      DConP (singledDataConName opts name) <$> mapM go pats
    go (ADTildeP pat) = do
      qReportWarning
        "Lazy pattern converted into regular pattern during singleton generation."
      go pat
    go (ADBangP pat) = DBangP <$> go pat
    go (ADSigP prom_pat pat ty) = do
      pat' <- go pat
      -- Normally, calling dPatToDExp would be dangerous, since it fails if the
      -- supplied pattern contains any wildcard patterns. However, promotePat
      -- (which produced the pattern we're passing into dPatToDExp) maintains
      -- an invariant that any promoted pattern signatures will be free of
      -- wildcard patterns in the underlying pattern.
      -- See Note [Singling pattern signatures].
      addElement (dPatToDExp pat', DSigT prom_pat ty)
      pure pat'
    go ADWildP = pure DWildP

-- | If given a non-empty list of 'SingDSigPaInfos', construct a case expression
-- that brings singleton equality constraints into scope via pattern-matching.
-- See @Note [Singling pattern signatures]@.
mkSigPaCaseE :: SingDSigPaInfos -> DExp -> DExp
mkSigPaCaseE exps_with_sigs exp
  | null exps_with_sigs = exp
  | otherwise =
      let (exps, sigs) = unzip exps_with_sigs
          scrutinee = mkTupleDExp exps
          pats = map (DSigP DWildP . DAppT (DConT singFamilyName)) sigs
      in DCaseE scrutinee [DMatch (mkTupleDPat pats) exp]

-- Note [Annotate case return type]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We're straining GHC's type inference here. One particular trouble area
-- is determining the return type of a GADT pattern match. In general, GHC
-- cannot infer return types of GADT pattern matches because the return type
-- becomes "untouchable" in the case matches. See the OutsideIn paper. But,
-- during singletonization, we *know* the return type. So, just add a type
-- annotation. See #54.
--
-- In particular, we add a type annotation in a somewhat unorthodox fashion.
-- Instead of the usual `(x :: t)`, we use `id @t x`. See
-- Note [The id hack; or, how singletons-th learned to stop worrying and avoid
-- kind generalization] for an explanation of why we do this.

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

-- Note [Singling pattern signatures]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We want to single a pattern signature, like so:
--
--   f :: Maybe a -> a
--   f (Just x :: Maybe a) = x
--
-- Naïvely, one might expect this to single straightfowardly as:
--
--   sF :: forall (z :: Maybe a). Sing z -> Sing (F z)
--   sF (SJust sX :: Sing (Just x :: Maybe a)) = sX
--
-- But the way GHC typechecks patterns prevents this from working, as GHC won't
-- know that the type `z` is actually `Just x` until /after/ the entirety of
-- the `SJust sX` pattern has been typechecked. (See Trac #12018 for an
-- extended discussion on this topic.)
--
-- To work around this design, we resort to a somewhat unsightly trick:
-- immediately after matching on all the patterns, we perform a case on every
-- pattern with a pattern signature, like so:
--
--   sF :: forall (z :: Maybe a). Sing z -> Sing (F z)
--   sF (SJust sX :: Sing z)
--     = case (SJust sX :: Sing z) of
--         (_ :: Sing (Just x :: Maybe a)) -> sX
--
-- Now GHC accepts the fact that `z` is `Just x`, and all is well. In order
-- to support this construction, the type of singPat is augmented with some
-- extra information in the form of SingDSigPaInfos:
--
--   type SingDSigPaInfos = [(DExp, DType)]
--
-- Where the DExps corresponds to the expressions we case on just after the
-- patterns (`SJust sX :: Sing x`, in the example above), and the DTypes
-- correspond to the singled pattern signatures to use in the case alternative
-- (`Sing (Just x :: Maybe a)` in the example above). singPat appends to the
-- list of SingDSigPaInfos whenever it processes a DSigPa (pattern signature),
-- and call sites can pass these SingDSigPaInfos to mkSigPaCaseE to construct a
-- case expression like the one featured above.
--
-- Some interesting consequences of this design:
--
-- 1. We must promote DPats to ADPats, a variation of DPat where the annotated
--    DSigPa counterpart, ADSigPa, stores the type that the original DPat was
--    promoted to. This is necessary since promoting the type might have
--    generated fresh variable names, so we need to be able to use the same
--    names when singling.
--
-- 2. Also when promoting a DSigPa to an ADSigPa, we remove any wildcards from
--    the underlying pattern. To see why this is necessary, consider singling
--    this example:
--
--      g (Just _ :: Maybe a) = "hi"
--
--    This must single to something like this:
--
--      sG (SJust _ :: Sing z)
--        = case (SJust _ :: Sing z) of
--            (_ :: Sing (Just _ :: Maybe a)) -> "hi"
--
--    But `SJust _` is not a valid expression, and since the minimal th-desugar
--    AST lacks as-patterns, we can't replace it with something like
--    `sG x@(SJust _ :: Sing z) = case x of ...`. But even if the th-desugar
--    AST /did/ have as-patterns, we'd still be in trouble, as `Just _` isn't
--    a valid type without the use of -XPartialTypeSignatures, which isn't a
--    design we want to force upon others.
--
--    We work around both issues by simply converting all wildcard patterns
--    from the pattern that has a signature. That means our example becomes:
--
--      sG (SJust sWild :: Sing z)
--        = case (SJust sWild :: Sing z) of
--            (_ :: Sing (Just wild :: Maybe a)) -> "hi"
--
--    And now everything is hunky-dory.

singExp :: ADExp -> Maybe DKind   -- the kind of the expression, if known
        -> SgM DExp
  -- See Note [Why error is so special]
singExp (ADVarE err `ADAppE` arg) _res_ki
  | err == errorName = do opts <- getOptions
                          DAppE (DVarE (singledValueName opts err)) <$>
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
  opts <- getOptions
  let sNames = map (singledValueName opts) names
  exp' <- singExp exp Nothing
  -- we need to bind the type variables... but DLamE doesn't allow SigT patterns.
  -- So: build a case
  let caseExp = DCaseE (mkTupleDExp (map DVarE sNames))
                       [DMatch (mkTupleDPat
                                (map ((DWildP `DSigP`) .
                                      (singFamily `DAppT`) .
                                      DVarT) ty_names)) exp']
  return $ wrapSingFun (length names) prom_lam $ DLamE sNames caseExp
singExp (ADCaseE exp matches ret_ty) res_ki =
    -- See Note [Annotate case return type] and
    --     Note [The id hack; or, how singletons-th learned to stop worrying and
    --           avoid kind generalization]
  DAppE (DAppTypeE (DVarE 'id)
                   (singFamily `DAppT` (ret_ty `maybeSigT` res_ki)))
    <$> (DCaseE <$> singExp exp Nothing <*> mapM (singMatch res_ki) matches)
singExp (ADLetE env exp) res_ki = do
  -- We intentionally discard the SingI instances for exp's defunctionalization
  -- symbols, as we also do not generate the declarations for the
  -- defunctionalization symbols in the first place during promotion.
  (let_decs, _, exp') <- singLetDecEnv env $ singExp exp res_ki
  pure $ DLetE let_decs exp'
singExp (ADSigE prom_exp exp ty) _ = do
  exp' <- singExp exp (Just ty)
  pure $ DSigE exp' $ DConT singFamilyName `DAppT` DSigT prom_exp ty

-- See Note [DerivedDecl] in Data.Singletons.Syntax
singDerivedEqDecs :: DerivedEqDecl -> SgM [DDec]
singDerivedEqDecs (DerivedDecl { ded_mb_cxt     = mb_ctxt
                               , ded_type       = ty
                               , ded_type_tycon = ty_tycon
                               , ded_decl       = DataDecl _ _ cons }) = do
  (scons, _) <- singM [] $ mapM (singCtor ty_tycon) cons
  mb_sctxt <- mapM (mapM singPred) mb_ctxt
  kind <- promoteType ty
  -- Beware! The user might have specified an instance context like this:
  --
  --   deriving instance Eq a => Eq (T a Int)
  --
  -- When we single the context, it will become (SEq a). But we do *not* want
  -- this for the SDecide instance! The simplest solution is to simply replace
  -- all occurrences of SEq with SDecide in the context.
  mb_sctxtDecide <- traverse (traverse sEqToSDecide) mb_sctxt
  sDecideInst <- mkDecideInstance mb_sctxtDecide kind cons scons
  testInsts <- traverse (mkTestInstance mb_sctxtDecide kind ty_tycon cons)
                        [TestEquality, TestCoercion]
  return (sDecideInst:testInsts)

-- Walk a DPred, replacing all occurrences of SEq with SDecide.
sEqToSDecide :: OptionsMonad q => DPred -> q DPred
sEqToSDecide p = do
  opts <- getOptions
  pure $ modifyConNameDType (\n ->
         if n == singledClassName opts eqName
            then sDecideClassName
            else n) p

-- See Note [DerivedDecl] in Data.Singletons.Syntax
singDerivedShowDecs :: DerivedShowDecl -> SgM [DDec]
singDerivedShowDecs (DerivedDecl { ded_mb_cxt     = mb_cxt
                                 , ded_type       = ty
                                 , ded_type_tycon = ty_tycon
                                 , ded_decl       = data_decl }) = do
    -- Generate a Show instance for a singleton type, like this:
    --
    --   instance (ShowSing a, ShowSing b) => Show (SEither (z :: Either a b)) where
    --     showsPrec p (SLeft (sl :: Sing l)) =
    --       showParen (p > 10) $ showString "SLeft " . showsPrec 11 sl
    --         :: ShowSing' l => ShowS
    --     showsPrec p (SRight (sr :: Sing r)) =
    --       showParen (p > 10) $ showString "SRight " . showsPrec 11 sr
    --         :: ShowSing' r => ShowS
    --
    -- Be careful: we want to generate an instance context that uses ShowSing,
    -- not SShow.
    show_sing_inst <- mkShowInstance (ForShowSing ty_tycon) mb_cxt ty data_decl
    pure [toInstanceD show_sing_inst]
  where
    toInstanceD :: UInstDecl -> DDec
    toInstanceD (InstDecl { id_cxt = cxt, id_name = inst_name
                          , id_arg_tys = inst_tys, id_meths = ann_meths }) =
      DInstanceD Nothing Nothing cxt (foldType (DConT inst_name) inst_tys)
                 (map (DLetDec . toFunD) ann_meths)

    toFunD :: (Name, ULetDecRHS) -> DLetDec
    toFunD (fun_name, UFunction clauses) = DFunD fun_name clauses
    toFunD (val_name, UValue rhs)        = DValD (DVarP val_name) rhs

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
  (sPat, sigPaExpsSigs) <- evalForPair $ singPat (Map.fromList var_proms) pat
  sExp <- singExp exp res_ki
  return $ DMatch sPat $ mkSigPaCaseE sigPaExpsSigs sExp

singLit :: Lit -> SgM DExp
singLit (IntegerL n) = do
  opts <- getOptions
  if n >= 0
     then return $
          DVarE (singledValueName opts fromIntegerName) `DAppE`
          (DVarE singMethName `DSigE`
           (singFamily `DAppT` DLitT (NumTyLit n)))
     else do sLit <- singLit (IntegerL (-n))
             return $ DVarE (singledValueName opts negateName) `DAppE` sLit
singLit (StringL str) = do
  opts <- getOptions
  let sing_str_lit = DVarE singMethName `DSigE`
                     (singFamily `DAppT` DLitT (StrTyLit str))
  os_enabled <- qIsExtEnabled LangExt.OverloadedStrings
  pure $ if os_enabled
         then DVarE (singledValueName opts fromStringName) `DAppE` sing_str_lit
         else sing_str_lit
singLit lit =
  fail ("Only string and natural number literals can be singled: " ++ show lit)

{-
Note [The id hack; or, how singletons-th learned to stop worrying and avoid kind generalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC 8.8 was a time of great change. In particular, 8.8 debuted a fix for
Trac #15141 (decideKindGeneralisationPlan is too complicated). To fix this, a
wily GHC developer—who shall remain unnamed, but whose username rhymes with
schmoldfire—decided to make decideKindGeneralisationPlan less complicated by,
well, removing the whole thing. One consequence of this is that local
definitions are now kind-generalized (whereas they would not have been
previously).

While schmoldfire had the noblest of intentions when authoring his fix, he
unintentionally made life much harder for singletons-th. Why? Consider the
following program:

  class Foo a where
    bar :: a -> (a -> b) -> b
    baz :: a

  quux :: Foo a => a -> a
  quux x = x `bar` \_ -> baz

When singled, this program will turn into something like this:

  type family Quux (x :: a) :: a where
    Quux x = Bar x (LambdaSym1 x)

  sQuux :: forall a (x :: a). SFoo a => Sing x -> Sing (Quux x :: a)
  sQuux (sX :: Sing x)
    = sBar sX
        ((singFun1 @(LambdaSym1 x))
           (\ sArg
              -> case sArg of {
                   (_ :: Sing arg)
                     -> (case sArg of { _ -> sBaz }) ::
                          Sing (Case x arg arg) }))

  type family Case x arg t where
    Case x arg _ = Baz
  type family Lambda x t where
    Lambda x arg = Case x arg arg
  data LambdaSym1 x t
  type instance Apply (LambdaSym1 x) t = Lambda x t

The high-level bit is the explicit `Sing (Case x arg arg)` signature. Question:
what is the kind of `Case x arg arg`? The answer depends on whether local
definitions are kind-generalized or not!

1. If local definitions are *not* kind-generalized (i.e., the status quo before
   GHC 8.8), then `Case x arg arg :: a`.
2. If local definitions *are* kind-generalized (i.e., the status quo in GHC 8.8
   and later), then `Case x arg arg :: k` for some fresh kind variable `k`.

Unfortunately, the kind of `Case x arg arg` *must* be `a` in order for `sQuux`
to type-check. This means that the code above suddenly stopped working in GHC
8.8. What's more, we can't just remove these explicit signatures, as there is
code elsewhere in `singletons-th` that crucially relies on them to guide type
inference along (e.g., `sShowParen` in `Data.Singletons.Prelude.Show`).

Luckily, there is an ingenious hack that lets us the benefits of explicit
signatures without the pain of kind generalization: our old friend, the `id`
function. The plan is as follows: instead of generating this code:

  (case sArg of ...) :: Sing (Case x arg arg)

We instead generate this code:

  id @(Sing (Case x arg arg)) (case sArg of ...)

That's it! This works because visible type arguments in terms do not get kind-
generalized, unlike top-level or local signatures. Now `Case x arg arg`'s kind
is not generalized, and all is well. We dub this: the `id` hack.

One might wonder: will we need the `id` hack around forever? Perhaps not. While
GHC 8.8 removed the decideKindGeneralisationPlan function, there have been
rumblings that a future version of GHC may bring it back (in a limited form).
If this happens, it is possibly that GHC's attitude towards kind-generalizing
local definitons may change *again*, which could conceivably render the `id`
hack unnecessary. This is all speculation, of course, so all we can do now is
wait and revisit this design at a later date.
-}
