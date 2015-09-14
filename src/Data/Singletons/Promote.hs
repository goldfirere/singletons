{- Data/Singletons/Promote.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to promote term-level constructs to the
type level. It is an internal module to the singletons package.
-}

{-# LANGUAGE TemplateHaskell, MultiWayIf, LambdaCase, TupleSections, CPP #-}

module Data.Singletons.Promote where

import Language.Haskell.TH hiding ( Q, cxt )
import Language.Haskell.TH.Syntax ( Quasi(..) )
import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Promote.Monad
import Data.Singletons.Promote.Eq
import Data.Singletons.Promote.Ord
import Data.Singletons.Promote.Bounded
import Data.Singletons.Promote.Defun
import Data.Singletons.Promote.Type
import Data.Singletons.Util
import Data.Singletons.Syntax
import Prelude hiding (exp)
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import Data.Maybe

-- | Generate promoted definitions from a type that is already defined.
-- This is generally only useful with classes.
genPromotions :: DsMonad q => [Name] -> q [Dec]
genPromotions names = do
  checkForRep names
  infos <- mapM reifyWithWarning names
  dinfos <- mapM dsInfo infos
  ddecs <- promoteM_ [] $ mapM_ promoteInfo dinfos
  return $ decsToTH ddecs

-- | Promote every declaration given to the type level, retaining the originals.
promote :: DsMonad q => q [Dec] -> q [Dec]
promote qdec = do
  decls <- qdec
  ddecls <- withLocalDeclarations decls $ dsDecs decls
  promDecls <- promoteM_ decls $ promoteDecs ddecls
  return $ decls ++ decsToTH promDecls

-- | Promote each declaration, discarding the originals.
promoteOnly :: DsMonad q => q [Dec] -> q [Dec]
promoteOnly qdec = do
  decls  <- qdec
  ddecls <- dsDecs decls
  promDecls <- promoteM_ decls $ promoteDecs ddecls
  return $ decsToTH promDecls

-- | Generate defunctionalization symbols for existing type family
genDefunSymbols :: DsMonad q => [Name] -> q [Dec]
genDefunSymbols names = do
  checkForRep names
  infos <- mapM (dsInfo <=< reifyWithWarning) names
  decs <- promoteMDecs [] $ concatMapM defunInfo infos
  return $ decsToTH decs

-- | Produce instances for '(:==)' (type-level equality) from the given types
promoteEqInstances :: DsMonad q => [Name] -> q [Dec]
promoteEqInstances = concatMapM promoteEqInstance

-- | Produce instances for 'Compare' from the given types
promoteOrdInstances :: DsMonad q => [Name] -> q [Dec]
promoteOrdInstances = concatMapM promoteOrdInstance

-- | Produce instances for 'MinBound' and 'MaxBound' from the given types
promoteBoundedInstances :: DsMonad q => [Name] -> q [Dec]
promoteBoundedInstances = concatMapM promoteBoundedInstance

-- | Produce an instance for '(:==)' (type-level equality) from the given type
promoteEqInstance :: DsMonad q => Name -> q [Dec]
promoteEqInstance name = do
  (_tvbs, cons) <- getDataD "I cannot make an instance of (:==) for it." name
  cons' <- mapM dsCon cons
  vars <- replicateM (length _tvbs) (qNewName "k")
  kind <- promoteType (foldType (DConT name) (map DVarT vars))
  inst_decs <- mkEqTypeInstance kind cons'
  return $ decsToTH inst_decs

-- | Produce an instance for 'Compare' from the given type
promoteOrdInstance :: DsMonad q => Name -> q [Dec]
promoteOrdInstance name = do
  (_tvbs, cons) <- getDataD "I cannot make an instance of Ord for it." name
  cons' <- mapM dsCon cons
  vars <- replicateM (length _tvbs) (qNewName "k")
  kind <- promoteType (foldType (DConT name) (map DVarT vars))
  inst_decs <- mkOrdTypeInstance kind cons'
  return $ decsToTH inst_decs

-- | Produce an instance for 'MinBound' and 'MaxBound' from the given type
promoteBoundedInstance :: DsMonad q => Name -> q [Dec]
promoteBoundedInstance name = do
  (_tvbs, cons) <- getDataD "I cannot make an instance of Bounded for it." name
  cons' <- mapM dsCon cons
  vars <- replicateM (length _tvbs) (qNewName "k")
  kind <- promoteType (foldType (DConT name) (map DVarT vars))
  inst_decs <- mkBoundedTypeInstance kind cons'
  return $ decsToTH inst_decs

promoteInfo :: DInfo -> PrM ()
promoteInfo (DTyConI dec _instances) = promoteDecs [dec]
promoteInfo (DPrimTyConI _name _numArgs _unlifted) =
  fail "Promotion of primitive type constructors not supported"
promoteInfo (DVarI _name _ty _mdec _fixity) =
  fail "Promotion of individual values not supported"
promoteInfo (DTyVarI _name _ty) =
  fail "Promotion of individual type variables not supported"

-- Note [Promoting declarations in two stages]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- It is necessary to know the types of things when promoting. So,
-- we promote in two stages: first, we build a LetDecEnv, which allows
-- for easy lookup. Then, we go through the actual elements of the LetDecEnv,
-- performing the promotion.
--
-- Why do we need the types? For kind annotations on the type family. We also
-- need to have both the types and the actual function definition at the same
-- time, because the function definition tells us how many patterns are
-- matched. Note that an eta-contracted function needs to return a TyFun,
-- not a proper type-level function.
--
-- Consider this example:
--
--   foo :: Nat -> Bool -> Bool
--   foo Zero = id
--   foo _    = not
--
-- Here the first parameter to foo is non-uniform, because it is
-- inspected in a pattern and can be different in each defining
-- equation of foo. The second parameter to foo, specified in the type
-- signature as Bool, is a uniform parameter - it is not inspected and
-- each defining equation of foo uses it the same way. The foo
-- function will be promoted to a type familty Foo like this:
--
--   type family Foo (n :: Nat) :: TyFun Bool Bool -> * where
--      Foo Zero = Id
--      Foo a    = Not
--
-- To generate type signature for Foo type family we must first learn
-- what is the actual number of patterns used in defining cequations
-- of foo. In this case there is only one so we declare Foo to take
-- one argument and have return type of Bool -> Bool.

-- Promote a list of top-level declarations.
promoteDecs :: [DDec] -> PrM ()
promoteDecs raw_decls = do
  decls <- expand raw_decls     -- expand type synonyms
  checkForRepInDecls decls
  PDecs { pd_let_decs              = let_decs
        , pd_class_decs            = classes
        , pd_instance_decs         = insts
        , pd_data_decs             = datas }    <- partitionDecs decls

    -- promoteLetDecs returns LetBinds, which we don't need at top level
  _ <- promoteLetDecs noPrefix let_decs
  mapM_ promoteClassDec classes
  let all_meth_sigs = foldMap (lde_types . cd_lde) classes
  mapM_ (promoteInstanceDec all_meth_sigs) insts
  promoteDataDecs datas

promoteDataDecs :: [DataDecl] -> PrM ()
promoteDataDecs data_decs = do
  rec_selectors <- concatMapM extract_rec_selectors data_decs
  _ <- promoteLetDecs noPrefix rec_selectors
  mapM_ promoteDataDec data_decs
  where
    extract_rec_selectors :: DataDecl -> PrM [DLetDec]
    extract_rec_selectors (DataDecl _nd data_name tvbs cons _derivings) =
      let arg_ty = foldType (DConT data_name)
                            (map (DVarT . extractTvbName) tvbs)
      in
      concatMapM (getRecordSelectors arg_ty) cons

-- curious about ALetDecEnv? See the LetDecEnv module for an explanation.
promoteLetDecs :: (String, String) -- (alpha, symb) prefixes to use
               -> [DLetDec] -> PrM ([LetBind], ALetDecEnv)
  -- See Note [Promoting declarations in two stages]
promoteLetDecs prefixes decls = do
  let_dec_env <- buildLetDecEnv decls
  all_locals <- allLocals
  let binds = [ (name, foldType (DConT sym) (map DVarT all_locals))
              | name <- Map.keys $ lde_defns let_dec_env
              , let proName = promoteValNameLhsPrefix prefixes name
                    sym = promoteTySym proName (length all_locals) ]
  (decs, let_dec_env') <- letBind binds $ promoteLetDecEnv prefixes let_dec_env
  emitDecs decs
  return (binds, let_dec_env' { lde_proms = Map.fromList binds })

-- Promotion of data types to kinds is automatic (see "Ginving Haskell a
-- Promotion" paper for more details). Here we "plug into" the promotion
-- mechanism to add some extra stuff to the promotion:
--
--  * if data type derives Eq we generate a type family that implements the
--    equality test for the data type.
--
--  * for each data constructor with arity greater than 0 we generate type level
--    symbols for use with Apply type family. In this way promoted data
--    constructors and promoted functions can be used in a uniform way at the
--    type level in the same way they can be used uniformly at the type level.
--
--  * for each nullary data constructor we generate a type synonym
promoteDataDec :: DataDecl -> PrM ()
promoteDataDec (DataDecl _nd name tvbs ctors derivings) = do
  -- deriving Eq instance
  _kvs <- replicateM (length tvbs) (qNewName "k")
  _kind <- promoteType (foldType (DConT name) (map DVarT _kvs))
  when (elem eqName derivings) $ do
    eq_decs <- mkEqTypeInstance _kind ctors
    emitDecs eq_decs

  -- deriving Ord instance
  when (elem ordName derivings) $ do
    ord_decs <- mkOrdTypeInstance _kind ctors
    emitDecs ord_decs

  -- deriving Bounded instance
  when (elem boundedName derivings) $ do
    bounded_decs <- mkBoundedTypeInstance _kind ctors
    emitDecs bounded_decs

  ctorSyms <- buildDefunSymsDataD name tvbs ctors
  emitDecs ctorSyms

promoteClassDec :: UClassDecl
                -> PrM AClassDecl
promoteClassDec decl@(ClassDecl { cd_cxt  = cxt
                                , cd_name = cls_name
                                , cd_tvbs = tvbs
                                , cd_fds  = fundeps
                                , cd_lde  = lde@LetDecEnv
                                    { lde_defns = defaults
                                    , lde_types = meth_sigs
                                    , lde_infix = infix_decls } }) = do
  let pClsName = promoteClassName cls_name
  (ptvbs, proxyCxt) <- mkKProxies (map extractTvbName tvbs)
  pCxt <- mapM promote_superclass_pred cxt
  let cxt'  = pCxt ++ proxyCxt
  sig_decs <- mapM (uncurry promote_sig) (Map.toList meth_sigs)
     -- the first arg to promoteMethod is a kind subst. We actually don't
     -- want to subst for default instances, so we pass Map.empty
  let defaults_list  = Map.toList defaults
      defaults_names = map fst defaults_list
  (default_decs, ann_rhss, prom_rhss)
    <- mapAndUnzip3M (promoteMethod Map.empty meth_sigs) defaults_list

  let infix_decls' = catMaybes $ map (uncurry promoteInfixDecl) infix_decls

  -- no need to do anything to the fundeps. They work as is!
  emitDecs [DClassD cxt' pClsName ptvbs fundeps
                    (sig_decs ++ default_decs ++ infix_decls')]
  let defaults_list' = zip defaults_names ann_rhss
      proms          = zip defaults_names prom_rhss
  return (decl { cd_lde = lde { lde_defns = Map.fromList defaults_list'
                              , lde_proms = Map.fromList proms } })
  where
    promote_sig :: Name -> DType -> PrM DDec
    promote_sig name ty = do
      let proName = promoteValNameLhs name
      (argKs, resK) <- promoteUnraveled ty
      args <- mapM (const $ qNewName "arg") argKs
      emitDecsM $ defunctionalize proName (map Just argKs) (Just resK)

      return $ DFamilyD TypeFam proName
                        (zipWith DKindedTV args argKs)
                        (Just resK)

    promote_superclass_pred :: DPred -> PrM DPred
    promote_superclass_pred = go
      where
      go (DAppPr pr ty) = DAppPr <$> go pr <*> fmap kindParam (promoteType ty)
      go (DSigPr pr _k) = go pr    -- just ignore the kind; it can't matter
      go (DVarPr name)  = fail $ "Cannot promote ConstraintKinds variables like "
                              ++ show name
      go (DConPr name)  = return $ DConPr (promoteClassName name)

-- returns (unpromoted method name, ALetDecRHS) pairs
promoteInstanceDec :: Map Name DType -> UInstDecl -> PrM AInstDecl
promoteInstanceDec meth_sigs
                   decl@(InstDecl { id_name     = cls_name
                                  , id_arg_tys  = inst_tys
                                  , id_meths    = meths }) = do
  cls_tvb_names <- lookup_cls_tvb_names
  inst_kis <- mapM promoteType inst_tys
  let subst = Map.fromList $ zip cls_tvb_names inst_kis
  (meths', ann_rhss, _) <- mapAndUnzip3M (promoteMethod subst meth_sigs) meths
  emitDecs [DInstanceD [] (foldType (DConT pClsName)
                                    (map kindParam inst_kis)) meths']
  return (decl { id_meths = zip (map fst meths) ann_rhss })
  where
    pClsName = promoteClassName cls_name

    lookup_cls_tvb_names :: PrM [Name]
    lookup_cls_tvb_names = do
      mb_info <- dsReify pClsName
      case mb_info of
        Just (DTyConI (DClassD _ _ tvbs _ _) _) -> return (map extract_kv_name tvbs)
        _ -> do
          mb_info' <- dsReify cls_name
          case mb_info' of
            Just (DTyConI (DClassD _ _ tvbs _ _) _) -> return (map extractTvbName tvbs)
            _ -> fail $ "Cannot find class declaration annotation for " ++ show cls_name

    extract_kv_name :: DTyVarBndr -> Name
    extract_kv_name (DKindedTV _ (DConK _kproxy [DVarK kv_name])) = kv_name
    extract_kv_name tvb = error $ "Internal error: extract_kv_name\n" ++ show tvb

-- promoteMethod needs to substitute in a method's kind because GHC does not do
-- enough kind checking of associated types. See GHC#9063. When that bug is fixed,
-- the substitution code can be removed.
-- Bug is fixed, but only in HEAD, naturally. When we stop supporting 7.8,
-- this can be rewritten more cleanly, I imagine.
-- UPDATE: GHC 7.10.2 didn't fully solve GHC#9063. Urgh.

promoteMethod :: Map Name DKind     -- instantiations for class tyvars
              -> Map Name DType     -- method types
              -> (Name, ULetDecRHS)
              -> PrM (DDec, ALetDecRHS, DType)
                 -- returns (type instance, ALetDecRHS, promoted RHS)
promoteMethod subst sigs_map (meth_name, meth_rhs) = do
  ((_, _, _, eqns), _defuns, ann_rhs)
    <- promoteLetDecRHS sigs_map noPrefix meth_name meth_rhs
  (arg_kis, res_ki) <- lookup_meth_ty
  meth_arg_tvs <- mapM (const $ qNewName "a") arg_kis
  let meth_arg_kis' = map (substKind subst) arg_kis
      meth_res_ki'  = substKind subst res_ki
      eqns'         = map (apply_kis meth_arg_kis' meth_res_ki') eqns
      helperNameBase = case nameBase proName of
                         first:_ | not (isHsLetter first) -> "TFHelper"
                         alpha                            -> alpha
  helperName <- newUniqueName helperNameBase
  emitDecs [DClosedTypeFamilyD helperName
                               (zipWith DKindedTV meth_arg_tvs meth_arg_kis')
                               (Just meth_res_ki') eqns']
  emitDecsM (defunctionalize helperName (map Just meth_arg_kis') (Just meth_res_ki'))
  return ( DTySynInstD
             proName
             (DTySynEqn (zipWith (DSigT . DVarT) meth_arg_tvs meth_arg_kis')
                        (foldType (DConT helperName) (map DVarT meth_arg_tvs)))
         , ann_rhs
         , DConT (promoteTySym helperName 0) )
  where
    proName = promoteValNameLhs meth_name

    lookup_meth_ty :: PrM ([DKind], DKind)
    lookup_meth_ty = case Map.lookup meth_name sigs_map of
      Nothing -> do
        mb_info <- dsReify proName
        case mb_info of
          Just (DTyConI (DFamilyD _ _ tvbs mb_res_ki) _)
            -> return ( map (default_to_star . extractTvbKind) tvbs
                      , default_to_star mb_res_ki )
          _ -> fail $ "Cannot find type annotation for " ++ show proName
      Just ty -> promoteUnraveled ty

    default_to_star Nothing  = DStarK
    default_to_star (Just k) = k

    apply_kis :: [DKind] -> DKind -> DTySynEqn -> DTySynEqn
    apply_kis arg_kis res_ki (DTySynEqn lhs rhs) =
      DTySynEqn (zipWith apply_ki lhs arg_kis) (apply_ki rhs res_ki)

    apply_ki :: DType -> DKind -> DType
    apply_ki = DSigT

promoteLetDecEnv :: (String, String) -> ULetDecEnv -> PrM ([DDec], ALetDecEnv)
promoteLetDecEnv prefixes (LetDecEnv { lde_defns = value_env
                                     , lde_types = type_env
                                     , lde_infix = infix_decls }) = do
  let infix_decls'  = catMaybes $ map (uncurry promoteInfixDecl) infix_decls

    -- promote all the declarations, producing annotated declarations
  let (names, rhss) = unzip $ Map.toList value_env
  (payloads, defun_decss, ann_rhss)
    <- fmap unzip3 $ zipWithM (promoteLetDecRHS type_env prefixes) names rhss

  emitDecs $ concat defun_decss
  let decs = map payload_to_dec payloads ++ infix_decls'

    -- build the ALetDecEnv
  let let_dec_env' = LetDecEnv { lde_defns = Map.fromList $ zip names ann_rhss
                               , lde_types = type_env
                               , lde_infix = infix_decls
                               , lde_proms = Map.empty }  -- filled in promoteLetDecs

  return (decs, let_dec_env')
  where
    payload_to_dec (name, tvbs, m_ki, eqns) = DClosedTypeFamilyD name tvbs m_ki eqns

promoteInfixDecl :: Fixity -> Name -> Maybe DDec
promoteInfixDecl fixity name
 | isUpcase name = Nothing   -- no need to promote the decl
 | otherwise     = Just $ DLetDec $ DInfixD fixity (promoteValNameLhs name)

-- This function is used both to promote class method defaults and normal
-- let bindings. Thus, it can't quite do all the work locally and returns
-- an intermediate structure. Perhaps a better design is available.
promoteLetDecRHS :: Map Name DType       -- local type env't
                 -> (String, String)     -- let-binding prefixes
                 -> Name                 -- name of the thing being promoted
                 -> ULetDecRHS           -- body of the thing
                 -> PrM ( (Name, [DTyVarBndr], Maybe DKind, [DTySynEqn]) -- "type family"
                        , [DDec]        -- defunctionalization
                        , ALetDecRHS )  -- annotated RHS
promoteLetDecRHS type_env prefixes name (UValue exp) = do
  (res_kind, num_arrows)
    <- case Map.lookup name type_env of
         Nothing -> return (Nothing, 0)
         Just ty -> do
           ki <- promoteType ty
           return (Just ki, countArgs ty)
  case num_arrows of
    0 -> do
      all_locals <- allLocals
      (exp', ann_exp) <- promoteExp exp
      let proName = promoteValNameLhsPrefix prefixes name
      defuns <- defunctionalize proName (map (const Nothing) all_locals) res_kind
      return ( ( proName, map DPlainTV all_locals, res_kind
               , [DTySynEqn (map DVarT all_locals) exp'] )
             , defuns
             , AValue (foldType (DConT proName) (map DVarT all_locals))
                      num_arrows ann_exp )
    _ -> do
      names <- replicateM num_arrows (newUniqueName "a")
      let pats    = map DVarPa names
          newArgs = map DVarE  names
      promoteLetDecRHS type_env prefixes name
                       (UFunction [DClause pats (foldExp exp newArgs)])

promoteLetDecRHS type_env prefixes name (UFunction clauses) = do
  numArgs <- count_args clauses
  (m_argKs, m_resK, ty_num_args) <- case Map.lookup name type_env of
    Nothing -> return (replicate numArgs Nothing, Nothing, numArgs)
    Just ty -> do
      -- promoteType turns arrows into TyFun. So, we unravel first to
      -- avoid this behavior. Note the use of ravelTyFun in resultK
      -- to make the return kind work out
      (argKs, resultK) <- promoteUnraveled ty
      -- invariant: countArgs ty == length argKs
      return (map Just argKs, Just resultK, length argKs)

  let proName = promoteValNameLhsPrefix prefixes name
  all_locals <- allLocals
  defun_decs <- defunctionalize proName
                (map (const Nothing) all_locals ++ m_argKs) m_resK
  let local_tvbs = map DPlainTV all_locals
  tyvarNames <- mapM (const $ qNewName "a") m_argKs
  expClauses <- mapM (etaExpand (ty_num_args - numArgs)) clauses
  (eqns, ann_clauses) <- mapAndUnzipM promoteClause expClauses
  prom_fun <- lookupVarE name
  let args     = zipWith inferMaybeKindTV tyvarNames m_argKs
      all_args = local_tvbs ++ args
  return ( (proName, all_args, m_resK, eqns)
         , defun_decs
         , AFunction prom_fun ty_num_args ann_clauses )

  where
    etaExpand :: Int -> DClause -> PrM DClause
    etaExpand n (DClause pats exp) = do
      names <- replicateM n (newUniqueName "a")
      let newPats = map DVarPa names
          newArgs = map DVarE  names
      return $ DClause (pats ++ newPats) (foldExp exp newArgs)

    count_args (DClause pats _ : _) = return $ length pats
    count_args _ = fail $ "Impossible! A function without clauses."

promoteClause :: DClause -> PrM (DTySynEqn, ADClause)
promoteClause (DClause pats exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  ((types, pats'), new_vars) <- evalForPair $ mapAndUnzipM promotePat pats
  (ty, ann_exp) <- lambdaBind new_vars $ promoteExp exp
  all_locals <- allLocals   -- these are bound *outside* of this clause
  return ( DTySynEqn (map DVarT all_locals ++ types) ty
         , ADClause new_vars pats' ann_exp )

promoteMatch :: DType -> DMatch -> PrM (DTySynEqn, ADMatch)
promoteMatch prom_case (DMatch pat exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  ((ty, pat'), new_vars) <- evalForPair $ promotePat pat
  (rhs, ann_exp) <- lambdaBind new_vars $ promoteExp exp
  all_locals <- allLocals
  return $ ( DTySynEqn (map DVarT all_locals ++ [ty]) rhs
           , ADMatch new_vars prom_case pat' ann_exp)

-- promotes a term pattern into a type pattern, accumulating bound variable names
-- See Note [No wildcards in singletons]
promotePat :: DPat -> QWithAux VarPromotions PrM (DType, DPat)
promotePat (DLitPa lit) = do
  lit' <- promoteLitPat lit
  return (lit', DLitPa lit)
promotePat (DVarPa name) = do
      -- term vars can be symbols... type vars can't!
  tyName <- mkTyName name
  addElement (name, tyName)
  return (DVarT tyName, DVarPa name)
promotePat (DConPa name pats) = do
  (types, pats') <- mapAndUnzipM promotePat pats
  let name' = unboxed_tuple_to_tuple name
  return (foldType (DConT name') types, DConPa name pats')
  where
    unboxed_tuple_to_tuple n
      | Just deg <- unboxedTupleNameDegree_maybe n = tupleDataName deg
      | otherwise                                  = n
promotePat (DTildePa pat) = do
  qReportWarning "Lazy pattern converted into regular pattern in promotion"
  (ty, pat') <- promotePat pat
  return (ty, DTildePa pat')
promotePat (DBangPa pat) = do
  qReportWarning "Strict pattern converted into regular pattern in promotion"
  (ty, pat') <- promotePat pat
  return (ty, DBangPa pat')
promotePat DWildPa = do
  name <- newUniqueName "_z"
  tyName <- mkTyName name
  addElement (name, tyName)
  return (DVarT tyName, DVarPa name)

promoteExp :: DExp -> PrM (DType, ADExp)
promoteExp (DVarE name) = fmap (, ADVarE name) $ lookupVarE name
promoteExp (DConE name) = return $ (promoteValRhs name, ADConE name)
promoteExp (DLitE lit)  = fmap (, ADLitE lit) $ promoteLitExp lit
promoteExp (DAppE exp1 exp2) = do
  (exp1', ann_exp1) <- promoteExp exp1
  (exp2', ann_exp2) <- promoteExp exp2
  return (apply exp1' exp2', ADAppE ann_exp1 ann_exp2)
promoteExp (DLamE names exp) = do
  lambdaName <- newUniqueName "Lambda"
  tyNames <- mapM mkTyName names
  let var_proms = zip names tyNames
  (rhs, ann_exp) <- lambdaBind var_proms $ promoteExp exp
  tyFamLamTypes <- mapM (const $ qNewName "t") names
  all_locals <- allLocals
  let all_args = all_locals ++ tyFamLamTypes
      tvbs     = map DPlainTV all_args
  emitDecs [DClosedTypeFamilyD lambdaName
                               tvbs
                               Nothing
                               [DTySynEqn (map DVarT (all_locals ++ tyNames))
                                          rhs]]
  emitDecsM $ defunctionalize lambdaName (map (const Nothing) all_args) Nothing
  let promLambda = foldl apply (DConT (promoteTySym lambdaName 0))
                               (map DVarT all_locals)
  return (promLambda, ADLamE var_proms promLambda names ann_exp)
promoteExp (DCaseE exp matches) = do
  caseTFName <- newUniqueName "Case"
  all_locals <- allLocals
  let prom_case = foldType (DConT caseTFName) (map DVarT all_locals)
  (exp', ann_exp)     <- promoteExp exp
  (eqns, ann_matches) <- mapAndUnzipM (promoteMatch prom_case) matches
  tyvarName  <- qNewName "t"
  let all_args = all_locals ++ [tyvarName]
      tvbs     = map DPlainTV all_args
  emitDecs [DClosedTypeFamilyD caseTFName tvbs Nothing eqns]
    -- See Note [Annotate case return type] in Single
  let applied_case = prom_case `DAppT` exp'
  return ( applied_case
         , ADCaseE ann_exp exp' ann_matches applied_case )
promoteExp (DLetE decs exp) = do
  unique <- qNewUnique
  let letPrefixes = uniquePrefixes "Let" ":<<<" unique
  (binds, ann_env) <- promoteLetDecs letPrefixes decs
  (exp', ann_exp) <- letBind binds $ promoteExp exp
  return (exp', ADLetE ann_env ann_exp)
promoteExp (DSigE exp ty) = do
  (exp', ann_exp) <- promoteExp exp
  ty' <- promoteType ty
  return (DSigT exp' ty', ADSigE ann_exp ty)
promoteExp e@(DStaticE _) = fail ("Static expressions cannot be promoted: " ++ show e)

promoteLitExp :: Monad m => Lit -> m DType
promoteLitExp (IntegerL n)
  | n >= 0    = return $ (DConT tyFromIntegerName `DAppT` DLitT (NumTyLit n))
  | otherwise = return $ (DConT tyNegateName `DAppT`
                          (DConT tyFromIntegerName `DAppT` DLitT (NumTyLit (-n))))
promoteLitExp (StringL str) = return $ DLitT (StrTyLit str)
promoteLitExp lit =
  fail ("Only string and natural number literals can be promoted: " ++ show lit)

promoteLitPat :: Monad m => Lit -> m DType
promoteLitPat (IntegerL n)
  | n >= 0    = return $ (DLitT (NumTyLit n))
  | otherwise =
    fail $ "Negative literal patterns are not allowed,\n" ++
           "because literal patterns are promoted to natural numbers."
promoteLitPat (StringL str) = return $ DLitT (StrTyLit str)
promoteLitPat lit =
  fail ("Only string and natural number literals can be promoted: " ++ show lit)
