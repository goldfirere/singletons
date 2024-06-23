{-# LANGUAGE TemplateHaskellQuotes #-}

{- Data/Singletons/TH/Single.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains functions to refine constructs to work with singleton
types. It is an internal module to the singletons-th package.
-}

module Data.Singletons.TH.Single where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (NameSpace(..), Quasi(..))
import Data.Singletons.TH.Deriving.Bounded
import Data.Singletons.TH.Deriving.Enum
import Data.Singletons.TH.Deriving.Eq
import Data.Singletons.TH.Deriving.Infer
import Data.Singletons.TH.Deriving.Ord
import Data.Singletons.TH.Deriving.Show
import Data.Singletons.TH.Deriving.Util
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Partition
import Data.Singletons.TH.Promote
import Data.Singletons.TH.Promote.Defun
import Data.Singletons.TH.Promote.Monad ( promoteM )
import Data.Singletons.TH.Promote.Type
import Data.Singletons.TH.Single.Data
import Data.Singletons.TH.Single.Decide
import Data.Singletons.TH.Single.Defun
import Data.Singletons.TH.Single.Fixity
import Data.Singletons.TH.Single.Monad
import Data.Singletons.TH.Single.Ord
import Data.Singletons.TH.Single.Type
import Data.Singletons.TH.Syntax
import Data.Singletons.TH.Util
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OMap.Strict as OMap
import Language.Haskell.TH.Desugar.OMap.Strict (OMap)
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
  -- in D.S.TH.Promote
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

-- | Create instances of 'SDecide', 'Eq', 'TestEquality', and 'TestCoercion' for
-- each type in the list.
singDecideInstances :: OptionsMonad q => [Name] -> q [Dec]
singDecideInstances = concatMapM singDecideInstance

-- | Create instances of 'SDecide', 'Eq', 'TestEquality', and 'TestCoercion' for
-- the given type.
singDecideInstance :: OptionsMonad q => Name -> q [Dec]
singDecideInstance name = do
  (_df, tvbs, cons) <- getDataD ("I cannot make an instance of SDecide for it.") name
  dtvbs <- mapM dsTvbVis tvbs
  let data_ty = foldTypeTvbs (DConT name) dtvbs
  dcons <- concatMapM (dsCon dtvbs data_ty) cons
  (scons, _) <- singM [] $ mapM (singCtor name) dcons
  sDecideInstance <- mkDecideInstance Nothing data_ty dcons scons
  eqInstance <- mkEqInstanceForSingleton data_ty name
  testInstances <- traverse (mkTestInstance Nothing data_ty name dcons)
                            [TestEquality, TestCoercion]
  return $ decsToTH (sDecideInstance:eqInstance:testInstances)

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
singShowInstance = singInstance mkShowInstance "Show"

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
  (df, tvbs, cons) <- getDataD ("I cannot make an instance of Show for it.") name
  dtvbs <- mapM dsTvbVis tvbs
  let data_ty = foldTypeTvbs (DConT name) dtvbs
  dcons <- concatMapM (dsCon dtvbs data_ty) cons
  let tyvars    = map (DVarT . extractTvbName) dtvbs
      kind      = foldType (DConT name) tyvars
      data_decl = DataDecl df name dtvbs dcons
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
singITyConInstances = mapM singITyConInstance

-- | Create an instance for @'SingI' TyCon{N}@, where @N@ is the positive
-- number provided as an argument.
--
-- Note that the generated code requires the use of the @QuantifiedConstraints@
-- language extension.
singITyConInstance :: DsMonad q => Int -> q Dec
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
           ctxt = [ DForallT (DForallInvis (map (`DPlainTV` SpecifiedSpec) as)) $
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
                            dLamE [DVarP x] $
                            DVarE withSingIName `DAppE` DVarE x
                                                `DAppE` DVarE singMethName]]

singInstance :: OptionsMonad q => DerivDesc q -> String -> Name -> q [Dec]
singInstance mk_inst inst_name name = do
  (df, tvbs, cons) <- getDataD ("I cannot make an instance of " ++ inst_name
                                ++ " for it.") name
  dtvbs <- mapM dsTvbVis tvbs
  let data_ty = foldTypeTvbs (DConT name) dtvbs
  dcons <- concatMapM (dsCon dtvbs data_ty) cons
  let data_decl = DataDecl df name dtvbs dcons
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
        , pd_derived_ord_decs        = derivedOrdDecs
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
                                 newDerivedOrdDecs <- concatMapM singDerivedOrdDecs derivedOrdDecs
                                 newDerivedShowDecs <- concatMapM singDerivedShowDecs derivedShowDecs
                                 return $ newDataDecls ++ newClassDecls
                                                       ++ newInstDecls
                                                       ++ newDerivedEqDecs
                                                       ++ newDerivedOrdDecs
                                                       ++ newDerivedShowDecs
    return $ promDecls ++ (map DLetDec newLetDecls) ++ singIDefunDecls ++ newDecls

-- see comment at top of file
buildDataLets :: OptionsMonad q => DataDecl -> q [(Name, DExp)]
buildDataLets (DataDecl _df _name _tvbs cons) = do
  opts <- getOptions
  fld_sels <- qIsExtEnabled LangExt.FieldSelectors
  pure $ concatMap (con_num_args opts fld_sels) cons
  where
    con_num_args :: Options -> Bool -> DCon -> [(Name, DExp)]
    con_num_args opts fld_sels (DCon _tvbs _cxt name fields _rty) =
      (name, wrapSingFun (length (tysOfConFields fields))
                         (DConT $ defunctionalizedName0 opts name)
                         (DConE $ singledDataConName opts name))
      : rec_selectors opts fld_sels fields

    rec_selectors :: Options -> Bool -> DConFields -> [(Name, DExp)]
    rec_selectors opts fld_sels con
      | fld_sels
      = case con of
          DNormalC {} -> []
          DRecC fields ->
            let names = map fstOf3 fields in
            [ (name, wrapSingFun 1 (DConT $ defunctionalizedName0 opts name)
                                   (DVarE $ singledValueName opts name))
            | name <- names ]

      | otherwise
      = []

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
                      , cd_lde  = LetDecEnv { lde_defns = default_defns
                                            , lde_types = meth_sigs
                                            , lde_infix = fixities
                                            , lde_proms = promoted_defaults } }) =
  bindContext [foldTypeTvbs (DConT cls_name) cls_tvbs] $ do
    opts <- getOptions
    mb_cls_sak <- dsReifyType cls_name
    let sing_cls_name   = singledClassName opts cls_name
        mb_sing_cls_sak = fmap (DKiSigD sing_cls_name) mb_cls_sak
        pro_meth_names_and_locals =
          map (\meth_name -> (promotedValueName opts meth_name Nothing, []))
              meth_names
    cls_infix_decls <- singReifiedInfixDecls $ cls_name:meth_names
    (sing_sigs, _, tyvar_names, cxts, res_kis, singIDefunss)
      <- unzip6 <$> zipWithM (singTySig no_meth_defns meth_sigs)
                             meth_names pro_meth_names_and_locals
    emitDecs $ maybeToList mb_sing_cls_sak ++ cls_infix_decls ++ concat singIDefunss
    let default_sigs = catMaybes $
                       zipWith4 (mk_default_sig opts) meth_names sing_sigs
                                                      tyvar_names res_kis
    sing_meths <- mapM (uncurry (singLetDecRHS (Map.fromList cxts)))
                       (OMap.assocs default_defns)
    fixities' <- mapMaybeM (uncurry singInfixDecl) $ OMap.assocs $ fmap fst fixities
    cls_cxt' <- mapM singPred cls_cxt
    return $ DClassD cls_cxt'
                     sing_cls_name
                     cls_tvbs
                     cls_fundeps   -- they are fine without modification
                     (map DLetDec (sing_sigs ++ sing_meths ++ fixities') ++ default_sigs)
  where
    no_meth_defns = error "Internal error: can't find declared method type"
    meth_names    = map fst $ OMap.assocs meth_sigs

    mk_default_sig :: Options -> Name -> DLetDec -> [Name] -> Maybe DType -> Maybe DDec
    mk_default_sig opts meth_name (DSigD s_name sty) bound_kvs (Just res_ki) =
      DDefaultSigD s_name <$> add_constraints opts meth_name sty bound_kvs res_ki
    mk_default_sig _ _ _ _ _ = error "Internal error: a singled signature isn't a signature."

    add_constraints :: Options -> Name -> DType -> [Name] -> DType -> Maybe DType
    -- We must look through `... :: Type` kind annotations, which can be added
    -- when singling type signatures lacking explicit `forall`s.
    -- See Note [Preserve the order of type variables during singling]
    -- (wrinkle 1) in D.S.TH.Single.Type.
    add_constraints opts meth_name (DSigT sty ski) bound_kvs res_ki = do
      sty' <- add_constraints opts meth_name sty bound_kvs res_ki
      pure $ DSigT sty' ski
    add_constraints opts meth_name sty bound_kvs res_ki = do
      (tvbs, cxt, args, res) <- unravelVanillaDType sty
      -- Class defaults always occur at the top level, and as such, they are
      -- guaranteed never to close over any local names.
      (prom_dflt_name, _prom_deflt_locals) <-
        OMap.lookup meth_name promoted_defaults

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
          prom_meth = DConT $ promotedValueName opts meth_name Nothing
          default_pred = foldType (DConT equalityName)
                                -- NB: Need the res_ki here to prevent ambiguous
                                -- kinds in result-inferred default methods.
                                -- See #175
                               [ foldType prom_meth tvs `DSigT` res_ki
                               , foldType (DConT prom_dflt_name) tvs ]
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
      let sing_meth_ty :: DType -> SgM DType
          sing_meth_ty inner_ty = do
            -- Make sure to expand through type synonyms here! Not doing so
            -- resulted in #167.
            raw_ty <- expand inner_ty
            (s_ty, _num_args, _tyvar_names, _ctxt, _arg_kis, _res_ki)
              <- singType (DConT $ promotedValueName opts name Nothing) raw_ty
            pure s_ty

      -- If an instance signature exists, single it. Otherwise, leave it out.
      -- Unlike most other places, we don't actually *need* explicit type
      -- signatures for instance methods, as GHC can figure out the types of
      -- the instance methods on its own. As such, any GADT pattern matching in
      -- the singled instance method code will work as expected.
      mb_s_ty <- traverse sing_meth_ty $ OMap.lookup name inst_sigs
      meth' <- singLetDecRHS
                 Map.empty -- Because we are singling an instance declaration,
                           -- we aren't generating defunctionalization symbols
                           -- for the class methods, and hence we aren't
                           -- generating any SingI instances. Therefore, we
                           -- don't need to include anything in this Map.
                 name rhs
      return $ map DLetDec
             $ maybeToList (DSigD (singledValueName opts name) <$> mb_s_ty)
            ++ [meth']

singLetDecEnv :: ALetDecEnv
              -> SgM a
              -> SgM ([DLetDec], [DDec], a)
                 -- Return:
                 --
                 -- 1. The singled let-decs
                 -- 2. SingI instances for any defunctionalization symbols
                 --    (see Data.Singletons.TH.Single.Defun)
                 -- 3. The result of running the `SgM a` action
singLetDecEnv (LetDecEnv { lde_defns = defns
                         , lde_types = types
                         , lde_infix = infix_decls
                         , lde_proms = proms })
              thing_inside = do
  let prom_list = OMap.assocs proms
  (typeSigs, letBinds, _tyvarNames, cxts, _res_kis, singIDefunss)
    <- unzip6 <$> mapM (uncurry (singTySig defns types)) prom_list
  infix_decls' <- mapMaybeM (uncurry singInfixDecl) $ OMap.assocs $ fmap fst infix_decls
  bindLets letBinds $ do
    let_decs <- mapM (uncurry (singLetDecRHS (Map.fromList cxts)))
                     (OMap.assocs defns)
    thing <- thing_inside
    return (infix_decls' ++ typeSigs ++ let_decs, concat singIDefunss, thing)

singTySig :: OMap Name ALetDecRHS  -- definitions
          -> OMap Name DType       -- type signatures
          -> Name -> LetDecProm    -- the LetDecProm is the promoted type, not the type sig!
          -> SgM ( DLetDec         -- the new type signature
                 , (Name, DExp)    -- the let-bind entry
                 , [Name]          -- the scoped tyvar names in the tysig
                 , (Name, DCxt)    -- the context of the type signature
                 , Maybe DKind     -- the result kind in the tysig
                 , [DDec]          -- SingI instances for defun symbols
                 )
singTySig defns types name (prom_name, prom_locals) = do
  opts <- getOptions
  let sName = singledValueName opts name
      prom_defun_name = defunctionalizedName0 opts prom_name
      prom_defun_ty   = foldTypeLocalVars (DConT prom_defun_name) prom_locals
  case OMap.lookup name types of
    Nothing -> do
      num_args <- guess_num_args
      (sty, tyvar_names) <- mk_sing_ty num_args
      singIDefuns <- singDefuns name VarName []
                                (map (const Nothing) tyvar_names) Nothing
      return ( DSigD sName sty
             , (name, wrapSingFun num_args prom_defun_ty (DVarE sName))
             , tyvar_names
             , (name, [])
             , Nothing
             , singIDefuns )
    Just ty -> do
      (sty, num_args, tyvar_names, ctxt, arg_kis, res_ki)
        <- singType prom_ty ty
      bound_cxt <- askContext
      singIDefuns <- singDefuns name VarName (bound_cxt ++ ctxt)
                                (map Just arg_kis) (Just res_ki)
      return ( DSigD sName sty
             , (name, wrapSingFun num_args prom_defun_ty (DVarE sName))
             , tyvar_names
             , (name, ctxt)
             , Just res_ki
             , singIDefuns )
  where
    prom_ty :: DType
    prom_ty = foldTypeLocalVars (DConT prom_name) prom_locals

    guess_num_args :: SgM Int
    guess_num_args =
      case OMap.lookup name defns of
        Nothing -> fail "Internal error: promotion known for something not let-bound."
        Just (AValue _) -> return 0
        Just (AFunction n _) -> return n

      -- create a Sing t1 -> Sing t2 -> ... type of a given arity and result type
    mk_sing_ty :: Int -> SgM (DType, [Name])
    mk_sing_ty n = do
      arg_names <- replicateM n (qNewName "arg")
      -- If there are no arguments, use `Sing @_` instead of `Sing`.
      -- See Note [Disable kind generalization for local functions if possible]
      let sing_w_wildcard | n == 0    = singFamily `DAppKindT` DWildCardT
                          | otherwise = singFamily
      return ( ravelVanillaDType
                 (map (`DPlainTV` SpecifiedSpec) arg_names)
                 []
                 (map (\nm -> singFamily `DAppT` DVarT nm) arg_names)
                 (sing_w_wildcard `DAppT`
                      (foldType prom_ty (map DVarT arg_names)))
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
To accomplish this, we use TypeApplications plus a wildcard type. That is, we
generate this code for `sF`:

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

singLetDecRHS :: Map Name DCxt    -- the context of the type signature
                                  -- (might not be known)
              -> Name -> ALetDecRHS -> SgM DLetDec
singLetDecRHS cxts name ld_rhs = do
  opts <- getOptions
  bindContext (Map.findWithDefault [] name cxts) $
    case ld_rhs of
      AValue exp ->
        DValD (DVarP (singledValueName opts name)) <$>
        singExp exp
      AFunction _num_arrows clauses ->
        DFunD (singledValueName opts name) <$>
              mapM singClause clauses

singClause :: ADClause -> SgM DClause
singClause (ADClause var_proms pats exp) = do
  opts <- getOptions
  (sPats, sigPaExpsSigs) <- evalForPair $ mapM (singPat (Map.fromList var_proms)) pats
  let lambda_binds = map (\(n,_) -> (n, singledValueName opts n)) var_proms
  sBody <- bindLambdas lambda_binds $ singExp exp
  return $ DClause sPats $ mkSigPaCaseE sigPaExpsSigs sBody

singPat :: Map Name LocalVar -- from term-level names to type-level names
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
                  Just (tyname, _) -> return tyname
      pure $ DVarP (singledValueName opts name)
               `DSigP` (singFamily `DAppT` DVarT tyname)
    go (ADConP name tys pats) = do
      opts <- getOptions
      DConP (singledDataConName opts name) tys <$> mapM go pats
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
mkSigPaCaseE exps_with_sigs exp =
  let (exps, sigs) = unzip exps_with_sigs
      pats = map (DSigP DWildP . DAppT (DConT singFamilyName)) sigs
  in multiCase exps pats exp

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

singExp :: ADExp -> SgM DExp
singExp (ADVarE name) = lookupVarE name
singExp (ADConE name) = lookupConE name
singExp (ADLitE lit)  = singLit lit
singExp (ADAppE e1 e2) = do
  e1' <- singExp e1
  e2' <- singExp e2
  pure $ DVarE applySingName `DAppE` e1' `DAppE` e2'
singExp (ADLamCasesE num_args prom_lam_cases clauses) = do
  clauses' <- traverse singClause clauses
  pure $ wrapSingFun num_args prom_lam_cases $ DLamCasesE clauses'
singExp (ADLetE env exp) = do
  -- We intentionally discard the SingI instances for exp's defunctionalization
  -- symbols, as we also do not generate the declarations for the
  -- defunctionalization symbols in the first place during promotion.
  (let_decs, _, exp') <- singLetDecEnv env $ singExp exp
  pure $ DLetE let_decs exp'
singExp (ADSigE prom_exp exp ty) = do
  exp' <- singExp exp
  pure $ DSigE exp' $ DConT singFamilyName `DAppT` DSigT prom_exp ty

-- See Note [DerivedDecl] in Data.Singletons.TH.Syntax
singDerivedEqDecs :: DerivedEqDecl -> SgM [DDec]
singDerivedEqDecs (DerivedDecl { ded_mb_cxt     = mb_ctxt
                               , ded_type       = ty
                               , ded_type_tycon = ty_tycon
                               , ded_decl       = DataDecl _ _ _ cons }) = do
  (scons, _) <- singM [] $ mapM (singCtor ty_tycon) cons
  mb_sctxt <- mapM (mapM singPred) mb_ctxt
  -- Beware! The user might have specified an instance context like this:
  --
  --   deriving instance Eq a => Eq (T a Int)
  --
  -- When we single the context, it will become (SEq a). But we do *not* want
  -- this for the SDecide instance! The simplest solution is to simply replace
  -- all occurrences of SEq with SDecide in the context.
  mb_sctxtDecide <- traverse (traverse sEqToSDecide) mb_sctxt
  sDecideInst <- mkDecideInstance mb_sctxtDecide ty cons scons
  eqInst <- mkEqInstanceForSingleton ty ty_tycon
  testInsts <- traverse (mkTestInstance mb_sctxtDecide ty ty_tycon cons)
                        [TestEquality, TestCoercion]
  return (sDecideInst:eqInst:testInsts)

-- Walk a DPred, replacing all occurrences of SEq with SDecide.
sEqToSDecide :: OptionsMonad q => DPred -> q DPred
sEqToSDecide p = do
  opts <- getOptions
  pure $ modifyConNameDType (\n ->
         if n == singledClassName opts eqName
            then sDecideClassName
            else n) p

-- See Note [DerivedDecl] in Data.Singletons.TH.Syntax
singDerivedOrdDecs :: DerivedOrdDecl -> SgM [DDec]
singDerivedOrdDecs (DerivedDecl { ded_type       = ty
                                , ded_type_tycon = ty_tycon }) = do
    ord_inst <- mkOrdInstanceForSingleton ty ty_tycon
    pure [ord_inst]

-- See Note [DerivedDecl] in Data.Singletons.TH.Syntax
singDerivedShowDecs :: DerivedShowDecl -> SgM [DDec]
singDerivedShowDecs (DerivedDecl { ded_mb_cxt     = mb_cxt
                                 , ded_type       = ty
                                 , ded_type_tycon = ty_tycon
                                 , ded_decl       = DataDecl _ _ _ cons }) = do
    opts <- getOptions
    z <- qNewName "z"
    -- Generate a Show instance for a singleton type, like this:
    --
    --   deriving instance (ShowSing a, ShowSing b) => Sing (SEither (z :: Either a b))
    --
    -- Be careful: we want to generate an instance context that uses ShowSing,
    -- not SShow.
    show_cxt <- inferConstraintsDef (fmap mkShowSingContext mb_cxt)
                                    (DConT showSingName)
                                    ty cons
    ki <- promoteType ty
    let sty_tycon = singledDataTypeName opts ty_tycon
        show_inst = DStandaloneDerivD Nothing Nothing show_cxt
                      (DConT showName `DAppT` (DConT sty_tycon `DAppT` DSigT (DVarT z) ki))
    pure [show_inst]

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
singLit (CharL c) =
  return $ DVarE singMethName `DSigE` (singFamily `DAppT` DLitT (CharTyLit c))
singLit lit =
  fail ("Only string, natural number, and character literals can be singled: " ++ show lit)
