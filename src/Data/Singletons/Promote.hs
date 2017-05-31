{- Data/Singletons/Promote.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains functions to promote term-level constructs to the
type level. It is an internal module to the singletons package.
-}

{-# LANGUAGE TemplateHaskell, MultiWayIf, LambdaCase, TupleSections #-}

module Data.Singletons.Promote where

import Language.Haskell.TH hiding ( Q, cxt )
import Language.Haskell.TH.Syntax ( Quasi(..) )
import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Promote.Monad
import Data.Singletons.Promote.Eq
import Data.Singletons.Promote.Defun
import Data.Singletons.Promote.Type
import Data.Singletons.Deriving.Ord
import Data.Singletons.Deriving.Bounded
import Data.Singletons.Deriving.Enum
import Data.Singletons.Partition
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

-- | Promote each declaration, discarding the originals. Note that a promoted
-- datatype uses the same definition as an original datatype, so this will
-- not work with datatypes. Classes, instances, and functions are all fine.
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

-- | Produce instances for 'POrd' from the given types
promoteOrdInstances :: DsMonad q => [Name] -> q [Dec]
promoteOrdInstances = concatMapM promoteOrdInstance

-- | Produce an instance for 'POrd' from the given type
promoteOrdInstance :: DsMonad q => Name -> q [Dec]
promoteOrdInstance = promoteInstance mkOrdInstance "Ord"

-- | Produce instances for 'PBounded' from the given types
promoteBoundedInstances :: DsMonad q => [Name] -> q [Dec]
promoteBoundedInstances = concatMapM promoteBoundedInstance

-- | Produce an instance for 'PBounded' from the given type
promoteBoundedInstance :: DsMonad q => Name -> q [Dec]
promoteBoundedInstance = promoteInstance mkBoundedInstance "Bounded"

-- | Produce instances for 'PEnum' from the given types
promoteEnumInstances :: DsMonad q => [Name] -> q [Dec]
promoteEnumInstances = concatMapM promoteEnumInstance

-- | Produce an instance for 'PEnum' from the given type
promoteEnumInstance :: DsMonad q => Name -> q [Dec]
promoteEnumInstance = promoteInstance mkEnumInstance "Enum"

-- | Produce an instance for '(:==)' (type-level equality) from the given type
promoteEqInstance :: DsMonad q => Name -> q [Dec]
promoteEqInstance name = do
  (_tvbs, cons) <- getDataD "I cannot make an instance of (:==) for it." name
  cons' <- concatMapM dsCon cons
  vars <- replicateM (length _tvbs) (qNewName "k")
  kind <- promoteType (foldType (DConT name) (map DVarT vars))
  inst_decs <- mkEqTypeInstance kind cons'
  return $ decsToTH inst_decs

promoteInstance :: DsMonad q => (DType -> [DCon] -> q UInstDecl)
                -> String -> Name -> q [Dec]
promoteInstance mk_inst class_name name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of " ++ class_name
                            ++ " for it.") name
  cons' <- concatMapM dsCon cons
  tvbs' <- mapM dsTvb tvbs
  raw_inst <- mk_inst (foldType (DConT name) (map tvbToType tvbs')) cons'
  decs <- promoteM_ [] $ void $ promoteInstanceDec Map.empty raw_inst
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
                            (map tvbToType tvbs)
      in
      getRecordSelectors arg_ty cons

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
  kvs <- replicateM (length tvbs) (qNewName "k")
  kind <- promoteType (foldType (DConT name) (map DVarT kvs))
  when (any (\case DConPr n -> n == eqName
                   _        -> False) derivings) $ do
    eq_decs <- mkEqTypeInstance kind ctors
    emitDecs eq_decs

  ctorSyms <- buildDefunSymsDataD name tvbs ctors
  emitDecs ctorSyms

-- Note [CUSKification]
-- ~~~~~~~~~~~~~~~~~~~~
-- GHC #12928 means that sometimes, this TH code will produce a declaration
-- that has a kind signature even when we want kind inference to work. There
-- seems to be no way to avoid this, so we embrace it:
--
--   * If a class type variable has no explicit kind, we make no effort to
--     guess it and default to *. This is OK because before TypeInType we were
--     limited by KProxy anyway.
--
--   * If a class type variable has an explicit kind, it is preserved.
--
-- This way, we always get proper CUSKs where we need them.

promoteClassDec :: UClassDecl
                -> PrM AClassDecl
promoteClassDec decl@(ClassDecl { cd_cxt  = cxt
                                , cd_name = cls_name
                                , cd_tvbs = tvbs'
                                , cd_fds  = fundeps
                                , cd_lde  = lde@LetDecEnv
                                    { lde_defns = defaults
                                    , lde_types = meth_sigs
                                    , lde_infix = infix_decls } }) = do
  let
    -- a workaround for GHC Trac #12928; see Note [CUSKification]
    cuskify :: DTyVarBndr -> DTyVarBndr
    cuskify (DPlainTV tvname) = DKindedTV tvname DStarT
    cuskify tv                = tv
    tvbs = map cuskify tvbs'
  let pClsName = promoteClassName cls_name
  pCxt <- mapM promote_superclass_pred cxt
  sig_decs <- mapM (uncurry promote_sig) (Map.toList meth_sigs)
  let defaults_list  = Map.toList defaults
      defaults_names = map fst defaults_list
  (default_decs, ann_rhss, prom_rhss)
    <- mapAndUnzip3M (promoteMethod Nothing meth_sigs) defaults_list

  let infix_decls' = catMaybes $ map (uncurry promoteInfixDecl) infix_decls

  -- no need to do anything to the fundeps. They work as is!
  emitDecs [DClassD pCxt pClsName tvbs fundeps
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

      return $ DOpenTypeFamilyD (DTypeFamilyHead proName
                                                 (zipWith DKindedTV args argKs)
                                                 (DKindSig resK)
                                                 Nothing)

    promote_superclass_pred :: DPred -> PrM DPred
    promote_superclass_pred = go
      where
      go (DAppPr pr ty) = DAppPr <$> go pr <*> promoteType ty
      go (DSigPr pr _k) = go pr    -- just ignore the kind; it can't matter
      go (DVarPr name)  = fail $ "Cannot promote ConstraintKinds variables like "
                              ++ show name
      go (DConPr name)  = return $ DConPr (promoteClassName name)
      go DWildCardPr    = return DWildCardPr

-- returns (unpromoted method name, ALetDecRHS) pairs
promoteInstanceDec :: Map Name DType -> UInstDecl -> PrM AInstDecl
promoteInstanceDec meth_sigs
                   decl@(InstDecl { id_name     = cls_name
                                  , id_arg_tys  = inst_tys
                                  , id_meths    = meths }) = do
  cls_tvb_names <- lookup_cls_tvb_names
  inst_kis <- mapM promoteType inst_tys
  let subst = Map.fromList $ zip cls_tvb_names inst_kis
  (meths', ann_rhss, _) <- mapAndUnzip3M (promoteMethod (Just subst) meth_sigs) meths
  emitDecs [DInstanceD Nothing [] (foldType (DConT pClsName)
                                    inst_kis) meths']
  return (decl { id_meths = zip (map fst meths) ann_rhss })
  where
    pClsName = promoteClassName cls_name

    lookup_cls_tvb_names :: PrM [Name]
    lookup_cls_tvb_names = do
      mb_info <- dsReify pClsName
      case mb_info of
        Just (DTyConI (DClassD _ _ tvbs _ _) _) -> return (map extractTvbName tvbs)
        _ -> do
          mb_info' <- dsReify cls_name
          case mb_info' of
            Just (DTyConI (DClassD _ _ tvbs _ _) _) -> return (map extractTvbName tvbs)
            _ -> fail $ "Cannot find class declaration annotation for " ++ show cls_name

-- promoteMethod needs to substitute in a method's kind because GHC does not do
-- enough kind checking of associated types. See GHC#9063. When that bug is fixed,
-- the substitution code can be removed.
-- Bug is fixed, but only in HEAD, naturally. When we stop supporting 7.8,
-- this can be rewritten more cleanly, I imagine.
-- UPDATE: GHC 7.10.2 didn't fully solve GHC#9063. Urgh.

promoteMethod :: Maybe (Map Name DKind)
                    -- ^ instantiations for class tyvars (Nothing for default decls)
              -> Map Name DType     -- method types
              -> (Name, ULetDecRHS)
              -> PrM (DDec, ALetDecRHS, DType)
                 -- returns (type instance, ALetDecRHS, promoted RHS)
promoteMethod m_subst sigs_map (meth_name, meth_rhs) = do
  (arg_kis, res_ki) <- lookup_meth_ty
  ((_, _, _, eqns), _defuns, ann_rhs)
    <- promoteLetDecRHS (Just (arg_kis, res_ki)) sigs_map noPrefix meth_name meth_rhs
  meth_arg_tvs <- mapM (const $ qNewName "a") arg_kis
  let do_subst      = maybe id substKind m_subst
      meth_arg_kis' = map do_subst arg_kis
      meth_res_ki'  = do_subst res_ki
      helperNameBase = case nameBase proName of
                         first:_ | not (isHsLetter first) -> "TFHelper"
                         alpha                            -> alpha
      family_args
    -- GHC 8 requires bare tyvars to the left of a type family default
        | Nothing <- m_subst
        = map DVarT meth_arg_tvs
        | otherwise
        = zipWith (DSigT . DVarT) meth_arg_tvs meth_arg_kis'
  helperName <- newUniqueName helperNameBase
  emitDecs [DClosedTypeFamilyD (DTypeFamilyHead
                                  helperName
                                  (zipWith DKindedTV meth_arg_tvs meth_arg_kis')
                                  (DKindSig meth_res_ki')
                                  Nothing)
                               eqns]
  emitDecsM (defunctionalize helperName (map Just meth_arg_kis') (Just meth_res_ki'))
  return ( DTySynInstD
             proName
             (DTySynEqn family_args
                        (foldApply (promoteValRhs helperName) (map DVarT meth_arg_tvs)))
         , ann_rhs
         , DConT (promoteTySym helperName 0) )
  where
    proName = promoteValNameLhs meth_name

    lookup_meth_ty :: PrM ([DKind], DKind)
    lookup_meth_ty = case Map.lookup meth_name sigs_map of
      Nothing -> do
        mb_info <- dsReify proName
        case mb_info of
          Just (DTyConI (DOpenTypeFamilyD (DTypeFamilyHead _ tvbs mb_res_ki _)) _)
            -> let arg_kis = map (default_to_star . extractTvbKind) tvbs
                   res_ki  = default_to_star (resultSigToMaybeKind mb_res_ki)
               in return (arg_kis, res_ki)
          _ -> fail $ "Cannot find type annotation for " ++ show proName
      Just ty -> promoteUnraveled ty

    default_to_star Nothing  = DStarT
    default_to_star (Just k) = k

promoteLetDecEnv :: (String, String) -> ULetDecEnv -> PrM ([DDec], ALetDecEnv)
promoteLetDecEnv prefixes (LetDecEnv { lde_defns = value_env
                                     , lde_types = type_env
                                     , lde_infix = infix_decls }) = do
  let infix_decls'  = catMaybes $ map (uncurry promoteInfixDecl) infix_decls

    -- promote all the declarations, producing annotated declarations
  let (names, rhss) = unzip $ Map.toList value_env
  (payloads, defun_decss, ann_rhss)
    <- fmap unzip3 $ zipWithM (promoteLetDecRHS Nothing type_env prefixes) names rhss

  emitDecs $ concat defun_decss
  let decs = map payload_to_dec payloads ++ infix_decls'

    -- build the ALetDecEnv
  let let_dec_env' = LetDecEnv { lde_defns = Map.fromList $ zip names ann_rhss
                               , lde_types = type_env
                               , lde_infix = infix_decls
                               , lde_proms = Map.empty }  -- filled in promoteLetDecs

  return (decs, let_dec_env')
  where
    payload_to_dec (name, tvbs, m_ki, eqns) = DClosedTypeFamilyD
                                                (DTypeFamilyHead name tvbs sig Nothing)
                                                eqns
      where
        sig = maybe DNoSig DKindSig m_ki

promoteInfixDecl :: Fixity -> Name -> Maybe DDec
promoteInfixDecl fixity name
 | isUpcase name = Nothing   -- no need to promote the decl
 | otherwise     = Just $ DLetDec $ DInfixD fixity (promoteValNameLhs name)

-- This function is used both to promote class method defaults and normal
-- let bindings. Thus, it can't quite do all the work locally and returns
-- an intermediate structure. Perhaps a better design is available.
promoteLetDecRHS :: Maybe ([DKind], DKind)  -- the promoted type of the RHS (if known)
                                            -- needed to fix #136
                 -> Map Name DType       -- local type env't
                 -> (String, String)     -- let-binding prefixes
                 -> Name                 -- name of the thing being promoted
                 -> ULetDecRHS           -- body of the thing
                 -> PrM ( (Name, [DTyVarBndr], Maybe DKind, [DTySynEqn]) -- "type family"
                        , [DDec]        -- defunctionalization
                        , ALetDecRHS )  -- annotated RHS
promoteLetDecRHS m_rhs_ki type_env prefixes name (UValue exp) = do
  (res_kind, num_arrows)
    <- case m_rhs_ki of
         Just (arg_kis, res_ki) -> return ( Just (ravelTyFun (arg_kis ++ [res_ki]))
                                          , length arg_kis )
         _ |  Just ty <- Map.lookup name type_env
           -> do ki <- promoteType ty
                 return (Just ki, countArgs ty)
           |  otherwise
           -> return (Nothing, 0)
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
      promoteLetDecRHS m_rhs_ki type_env prefixes name
                       (UFunction [DClause pats (foldExp exp newArgs)])

promoteLetDecRHS m_rhs_ki type_env prefixes name (UFunction clauses) = do
  numArgs <- count_args clauses
  (m_argKs, m_resK, ty_num_args) <- case m_rhs_ki of
    Just (arg_kis, res_ki) -> return (map Just arg_kis, Just res_ki, length arg_kis)
    _ |  Just ty <- Map.lookup name type_env
      -> do
      -- promoteType turns arrows into TyFun. So, we unravel first to
      -- avoid this behavior. Note the use of ravelTyFun in resultK
      -- to make the return kind work out
      (argKs, resultK) <- promoteUnraveled ty
      -- invariant: countArgs ty == length argKs
      return (map Just argKs, Just resultK, length argKs)

      |  otherwise
      -> return (replicate numArgs Nothing, Nothing, numArgs)
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
  (types, new_vars) <- evalForPair $ mapM promotePat pats
  (ty, ann_exp) <- lambdaBind new_vars $ promoteExp exp
  all_locals <- allLocals   -- these are bound *outside* of this clause
  return ( DTySynEqn (map DVarT all_locals ++ types) ty
         , ADClause new_vars pats ann_exp )

promoteMatch :: DMatch -> PrM (DTySynEqn, ADMatch)
promoteMatch (DMatch pat exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  (ty, new_vars) <- evalForPair $ promotePat pat
  (rhs, ann_exp) <- lambdaBind new_vars $ promoteExp exp
  all_locals <- allLocals
  return $ ( DTySynEqn (map DVarT all_locals ++ [ty]) rhs
           , ADMatch new_vars pat ann_exp)

-- promotes a term pattern into a type pattern, accumulating bound variable names
promotePat :: DPat -> QWithAux VarPromotions PrM DType
promotePat (DLitPa lit) = do
  lit' <- promoteLitPat lit
  return lit'
promotePat (DVarPa name) = do
      -- term vars can be symbols... type vars can't!
  tyName <- mkTyName name
  addElement (name, tyName)
  return $ DVarT tyName
promotePat (DConPa name pats) = do
  types <- mapM promotePat pats
  let name' = unboxed_tuple_to_tuple name
  return $ foldType (DConT name') types
  where
    unboxed_tuple_to_tuple n
      | Just deg <- unboxedTupleNameDegree_maybe n = tupleDataName deg
      | otherwise                                  = n
promotePat (DTildePa pat) = do
  qReportWarning "Lazy pattern converted into regular pattern in promotion"
  promotePat pat
promotePat (DBangPa pat) = do
  qReportWarning "Strict pattern converted into regular pattern in promotion"
  promotePat pat
promotePat (DSigPa pat ty) = do
  promoted <- promotePat pat
  ki <- promoteType ty
  return $ DSigT promoted ki
promotePat DWildPa = do
  name <- newUniqueName "_z"
  tyName <- mkTyName name
  return $ DVarT tyName

promoteExp :: DExp -> PrM (DType, ADExp)
promoteExp (DVarE name) = fmap (, ADVarE name) $ lookupVarE name
promoteExp (DConE name) = return $ (promoteValRhs name, ADConE name)
promoteExp (DLitE lit)  = fmap (, ADLitE lit) $ promoteLitExp lit
promoteExp (DAppE exp1 exp2) = do
  (exp1', ann_exp1) <- promoteExp exp1
  (exp2', ann_exp2) <- promoteExp exp2
  return (apply exp1' exp2', ADAppE ann_exp1 ann_exp2)
-- Until we get visible kind applications, this is the best we can do.
promoteExp (DAppTypeE exp _) = promoteExp exp
promoteExp (DLamE names exp) = do
  lambdaName <- newUniqueName "Lambda"
  tyNames <- mapM mkTyName names
  let var_proms = zip names tyNames
  (rhs, ann_exp) <- lambdaBind var_proms $ promoteExp exp
  tyFamLamTypes <- mapM (const $ qNewName "t") names
  all_locals <- allLocals
  let all_args = all_locals ++ tyFamLamTypes
      tvbs     = map DPlainTV all_args
  emitDecs [DClosedTypeFamilyD (DTypeFamilyHead
                                 lambdaName
                                 tvbs
                                 DNoSig
                                 Nothing)
                               [DTySynEqn (map DVarT (all_locals ++ tyNames))
                                          rhs]]
  emitDecsM $ defunctionalize lambdaName (map (const Nothing) all_args) Nothing
  let promLambda = foldl apply (DConT (promoteTySym lambdaName 0))
                               (map DVarT all_locals)
  return (promLambda, ADLamE tyNames promLambda names ann_exp)
promoteExp (DCaseE exp matches) = do
  caseTFName <- newUniqueName "Case"
  all_locals <- allLocals
  let prom_case = foldType (DConT caseTFName) (map DVarT all_locals)
  (exp', ann_exp)     <- promoteExp exp
  (eqns, ann_matches) <- mapAndUnzipM promoteMatch matches
  tyvarName  <- qNewName "t"
  let all_args = all_locals ++ [tyvarName]
      tvbs     = map DPlainTV all_args
  emitDecs [DClosedTypeFamilyD (DTypeFamilyHead caseTFName tvbs DNoSig Nothing) eqns]
    -- See Note [Annotate case return type] in Single
  let applied_case = prom_case `DAppT` exp'
  return ( applied_case
         , ADCaseE ann_exp ann_matches applied_case )
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
