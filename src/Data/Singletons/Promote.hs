{- Data/Singletons/Promote.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to promote term-level constructs to the
type level. It is an internal module to the singletons package.
-}

{-# LANGUAGE TemplateHaskell, CPP, MultiWayIf, LambdaCase, TupleSections #-}

module Data.Singletons.Promote where

import Language.Haskell.TH hiding ( Q, cxt )
import Language.Haskell.TH.Syntax ( Quasi(..) )
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Sweeten
import Data.Singletons.Names
import Data.Singletons.Promote.Monad
import Data.Singletons.Promote.Eq
import Data.Singletons.Promote.Defun
import Data.Singletons.Promote.Type
import Data.Singletons.Util
import Data.Singletons.Syntax
import Prelude hiding (exp)
import Control.Monad
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )

-- | Generate promoted definitions from a type that is already defined.
-- This is generally only useful with classes.
genPromotions :: Quasi q => [Name] -> q [Dec]
genPromotions names = do
  checkForRep names
  infos <- mapM reifyWithWarning names
  dinfos <- mapM dsInfo infos
  ddecs <- promoteM_ $ mapM_ promoteInfo dinfos
  return $ decsToTH ddecs

-- | Promote every declaration given to the type level, retaining the originals.
promote :: Quasi q => q [Dec] -> q [Dec]
promote qdec = do
  decls <- qdec
  ddecls <- dsDecs decls
  promDecls <- promoteM_ $ promoteDecs ddecls
  return $ decls ++ decsToTH promDecls

-- | Promote each declaration, discarding the originals.
promoteOnly :: Quasi q => q [Dec] -> q [Dec]
promoteOnly qdec = do
  decls  <- qdec
  ddecls <- dsDecs decls
  promDecls <- promoteM_ $ promoteDecs ddecls
  return $ decsToTH promDecls

-- | Generate defunctionalization symbols for existing type family
genDefunSymbols :: Quasi q => [Name] -> q [Dec]
genDefunSymbols names = do
  checkForRep names
  infos <- mapM (dsInfo <=< reifyWithWarning) names
  decs <- promoteMDecs $ concatMapM defunInfo infos
  return $ decsToTH decs

-- | Produce instances for '(:==)' (type-level equality) from the given types
promoteEqInstances :: Quasi q => [Name] -> q [Dec]
promoteEqInstances = concatMapM promoteEqInstance

-- | Produce an instance for '(:==)' (type-level equality) from the given type
promoteEqInstance :: Quasi q => Name -> q [Dec]
promoteEqInstance name = do
  (_tvbs, cons) <- getDataD "I cannot make an instance of (:==) for it." name
  cons' <- mapM dsCon cons
#if __GLASGOW_HASKELL__ >= 707
  vars <- replicateM (length _tvbs) (qNewName "k")
  let tyvars = map DVarK vars
      kind = DConK name tyvars
  inst_decs <- mkEqTypeInstance kind cons'
  return $ decsToTH inst_decs
#else
  let pairs = [(c1, c2) | c1 <- cons, c2 <- cons]
  mapM (fmap decsToTH . mkEqTypeInstance) pairs
#endif

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
promoteDecs decls = do
  checkForRepInDecls decls
  -- See Note [Promoting declarations in two stages]
  PDecs { pd_let_decs              = let_decs
        , pd_class_decs            = classes
        , pd_instance_decs         = insts
        , pd_data_decs             = datas }    <- partitionDecs decls

    -- promoteLetDecs returns LetBinds, which we don't need at top level
  _ <- promoteLetDecs noPrefix let_decs
  (class_tvbs, meth_sigss) <- mapAndUnzipM promoteClassDec classes
  mapM_ (promoteInstanceDec (Map.fromList class_tvbs)
                            (Map.fromList $ concat meth_sigss)) insts
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
promoteLetDecs :: String -- prefix to use on all new definitions
               -> [DLetDec] -> PrM ([LetBind], ALetDecEnv)
promoteLetDecs prefix decls = do
  let_dec_env <- buildLetDecEnv decls
  all_locals <- allLocals
  let binds = [ (name, foldType (DConT sym) (map DVarT all_locals))
              | name <- Map.keys $ lde_defns let_dec_env
              , let proName = promoteValNameLhsPrefix prefix name
                    sym = promoteTySym proName (length all_locals) ]
  (decs, let_dec_env') <- letBind binds $ promoteLetDecEnv prefix let_dec_env
  emitDecs decs 
  return (binds, let_dec_env' { lde_proms = Map.fromList binds })

noPrefix :: String
noPrefix = ""

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
#if __GLASGOW_HASKELL__ < 707
  when (_nd == Newtype) $
    fail $ "Newtypes don't promote under GHC 7.6. " ++
           "Use <<data>> instead or upgrade GHC."
#endif
  when (elem eqName derivings) $ do
#if __GLASGOW_HASKELL__ >= 707
    kvs <- replicateM (length tvbs) (qNewName "k")
    inst_decs <- mkEqTypeInstance (DConK name (map DVarK kvs)) ctors
#else
    let pairs = [ (c1, c2) | c1 <- ctors, c2 <- ctors ]
    inst_decs <- mapM mkEqTypeInstance pairs
#endif
    emitDecs inst_decs
  ctorSyms <- buildDefunSymsDataD name tvbs ctors
  emitDecs ctorSyms

-- returns mappings from method names to pairs of types with the bound
-- names, appearing in the order of the class declaration
promoteClassDec :: ClassDecl -> PrM ( (Name, [Name])    -- from cls_name to tvbs
                                    , [(Name, DType)] ) -- method sigs
promoteClassDec (ClassDecl name tvbs sigs) = do
  -- we need to remember the names of the tvbs so we can perform substitution
  -- on instance method declarations.
  -- But, we need a way to do this non-locally. The solution? Spit out a
  -- fake type class declaration with just the right tvbs. Then, use the
  -- SuppressUnusedWarnings nonsense to suppress the warning. Ugh.
  let tvsName  = classTvsName name
      tvbNames = map extractTvbName tvbs
  emitDecs [ DClassD [] tvsName tvbs [] []
           , DInstanceD [] (DConT suppressClassName `DAppT` DConT tvsName)
                        [DLetDec (DFunD suppressMethodName
                                        [DClause [DWildPa] (mkTupleDExp [])])]]
  mapM_ promoteMethSig sigs
  return ( (name, tvbNames)
         , sigs )

promoteMethSig :: (Name, DType) -> PrM ()
promoteMethSig (name, ty) = do
  -- the real ty also has a context from the class. But, we ignore conexts anyway.
  let proName = promoteValNameLhs name
      (_, all_tys) = unravel ty
  all_kis <- mapM promoteType all_tys
  let (arg_kis, res_ki) = snocView all_kis
  emitDecsM $ defunctionalize proName (map Just arg_kis) (Just res_ki)
  arg_names <- mapM (const $ qNewName "arg") arg_kis
  emitDecs [DFamilyD TypeFam
                     proName
                     (zipWith DKindedTV arg_names arg_kis)
                     (Just res_ki)]

promoteInstanceDec :: Map Name [Name]
                   -> Map Name DType -> InstDecl -> PrM ()
promoteInstanceDec cls_tvbs meth_tys (InstDecl cls_name inst_tys meths) = do
  tvbNames <- lookup_cls_tvbs
  inst_kis <- mapM promoteType inst_tys
  let subst = Map.fromList $ zip (map nameBase tvbNames) inst_kis
  mapM_ (promoteMethDefn subst meth_tys) meths
  where
    lookup_cls_tvbs :: PrM [Name]
    lookup_cls_tvbs = case Map.lookup cls_name cls_tvbs of
      Nothing -> do
        m_dinfo <- qReifyMaybe (classTvsName cls_name)
        case m_dinfo of
          Just (DTyConI (DClassD _cxt _name tvbs _fds _decs) _insts) ->
            return $ map extractTvbName tvbs
          _ -> fail $ "Cannot find class declaration for " ++ show cls_name
      Just names -> return names
  
promoteMethDefn :: Map String DKind
                -> Map Name DType -> (Name, ULetDecRHS) -> PrM ()
promoteMethDefn subst meth_tys (name, let_rhs) = do
  (meth_arg_kis, meth_res_ki) <- lookup_meth_ty
  let meth_arg_kis' = map subst_ki meth_arg_kis
      meth_res_ki'  = subst_ki meth_res_ki
  eqns <- case let_rhs of
    UValue exp -> do    
      (exp', _) <- promoteExp exp
      return [DTySynEqn [] exp']
    UFunction clauses -> fmap fst $ mapAndUnzipM promoteClause clauses
  eqns' <- mapM (eta_expand (length meth_arg_kis')) eqns
  let eqns'' = map (apply_kis meth_arg_kis' meth_res_ki') eqns'
  emitDecs $ map (DTySynInstD proName) eqns''
  where
    proName = promoteValNameLhs name
    
    lookup_meth_ty :: PrM ([DKind], DKind)
    lookup_meth_ty = case Map.lookup name meth_tys of
      Nothing -> do
          -- lookup the promoted name, just in case the term-level one
          -- isn't defined
        m_dinfo <- qReifyMaybe proName
        case m_dinfo of
          Just (DTyConI (DFamilyD _flav _name tvbs (Just res)) _insts) -> do
            arg_kis <- mapM (expect_just . extractTvbKind) tvbs
            return (arg_kis, res)
          _ -> fail $ "Cannot find type of " ++ show proName
      Just ty -> do
        let (_, tys) = unravel ty
        kis <- mapM promoteType tys
        return $ snocView kis

    expect_just :: Maybe a -> PrM a
    expect_just (Just x) = return x
    expect_just Nothing =
      fail "Internal error: unknown kind of a promoted class method."

    eta_expand :: Int -> DTySynEqn -> PrM DTySynEqn
    eta_expand needed_num_args eqn@(DTySynEqn lhs rhs) =
      let orig_num_args = length lhs in
      if orig_num_args == needed_num_args
      then return eqn
      else do
        let num_extra_args = needed_num_args - orig_num_args
        new_names <- replicateM num_extra_args (qNewName "eta_arg")
        let new_tys = map DVarT new_names
            lhs'    = lhs ++ new_tys
            rhs'    = foldl apply rhs new_tys
        return $ DTySynEqn lhs' rhs'

    subst_ki :: DKind -> DKind
    subst_ki (DForallK {}) =
      error "Higher-rank kind encountered in instance method promotion."
    subst_ki (DVarK n) =
      case Map.lookup (nameBase n) subst of
        Just ki -> ki
        Nothing -> DVarK n
    subst_ki (DConK con kis) = DConK con (map subst_ki kis)
    subst_ki (DArrowK k1 k2) = DArrowK (subst_ki k1) (subst_ki k2)
    subst_ki DStarK = DStarK

    apply_kis :: [DKind] -> DKind -> DTySynEqn -> DTySynEqn
    apply_kis arg_kis res_ki (DTySynEqn lhs rhs) =
      DTySynEqn (zipWith apply_ki lhs arg_kis) (apply_ki rhs res_ki)

    apply_ki :: DType -> DKind -> DType
    apply_ki = DSigT
      

promoteLetDecEnv :: String -> ULetDecEnv -> PrM ([DDec], ALetDecEnv)
promoteLetDecEnv prefix (LetDecEnv { lde_defns = value_env
                                   , lde_types = type_env
                                   , lde_infix = infix_decls }) = do
    -- deal with the infix_decls, to get them out of the way
  let infix_decls'  = catMaybes $ map (uncurry prom_infix_decl) infix_decls

    -- promote all the declarations, producing annotated declarations
      (names, rhss) = unzip $ Map.toList value_env
  (decs, ann_rhss) <- fmap unzip $ zipWithM prom_let_dec names rhss

    -- build the ALetDecEnv
  let let_dec_env' = LetDecEnv { lde_defns = Map.fromList $ zip names ann_rhss
                               , lde_types = type_env
                               , lde_infix = infix_decls
                               , lde_proms = Map.empty }  -- filled in promoteLetDecs

  return (infix_decls' ++ decs, let_dec_env')
  where
    prom_infix_decl :: Fixity -> Name -> Maybe DDec
    prom_infix_decl fixity name
      | isUpcase name = Nothing   -- no need to promote the decl
      | otherwise     = Just $ DLetDec $ DInfixD fixity (promoteValNameLhs name)
    
    prom_let_dec :: Name -> ULetDecRHS -> PrM (DDec, ALetDecRHS)
    prom_let_dec name (UValue exp) = do
      all_locals <- allLocals
      (exp', ann_exp) <- promoteExp exp
      let proName = promote_lhs name
      (res_kind, mk_rhs, num_arrows)
        <- case Map.lookup name type_env of
             Nothing -> return (Nothing, id, 0)
             Just ty -> do
               ki <- promoteType ty
               return (Just ki, (`DSigT` ki), countArgs ty)
      emitDecsM $ defunctionalize proName (map (const Nothing) all_locals) res_kind
      return ( DTySynD proName (map DPlainTV all_locals) (mk_rhs exp')
             , AValue (foldType (DConT proName) (map DVarT all_locals))
                      num_arrows ann_exp )
        
    prom_let_dec name (UFunction clauses) = do
      numArgs <- count_args clauses
      (m_argKs, m_resK, ty_num_args) <- case Map.lookup name type_env of
#if __GLASGOW_HASKELL__ < 707
          -- we require a type signature here because GHC 7.6.3 doesn't support
          -- kind inference for type families
        Nothing -> fail ("No type signature for function \"" ++
                         (nameBase name) ++ "\". Cannot promote in GHC 7.6.3.\n" ++
                         "Either add a type signature or upgrade GHC.")
#else
        Nothing -> return (replicate numArgs Nothing, Nothing, numArgs)
#endif
        Just ty -> do
          -- promoteType turns arrows into TyFun. So, we unravel first to
          -- avoid this behavior. Note the use of ravelTyFun in resultK
          -- to make the return kind work out
          kis <- mapM promoteType (snd $ unravel ty)
          let (argKs, resultKs) = splitAt numArgs kis
              resultK = ravelTyFun resultKs
          return (map Just argKs, Just resultK, countArgs ty)
    
      let proName = promote_lhs name
      all_locals <- allLocals
      emitDecsM $ defunctionalize proName
                    (map (const Nothing) all_locals ++ m_argKs) m_resK
      local_tvbs <- mapM inferKindTV all_locals
      tyvarNames <- mapM (const $ qNewName "a") m_argKs
      (eqns, ann_clauses) <- mapAndUnzipM promoteClause clauses
      prom_fun <- lookupVarE name
      args <- zipWithM inferMaybeKindTV tyvarNames m_argKs
      let all_args = local_tvbs ++ args
      resultK <- inferKind m_resK
      return ( DClosedTypeFamilyD proName all_args resultK eqns
             , AFunction prom_fun ty_num_args ann_clauses )

    promote_lhs = promoteValNameLhsPrefix prefix

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

promoteMatch :: DType -> DMatch -> PrM (DTySynEqn, ADMatch)
promoteMatch prom_case (DMatch pat exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  (ty, new_vars) <- evalForPair $ promotePat pat
  (rhs, ann_exp) <- lambdaBind new_vars $ promoteExp exp
  all_locals <- allLocals
  return $ ( DTySynEqn (map DVarT all_locals ++ [ty]) rhs
           , ADMatch new_vars prom_case pat ann_exp)

-- promotes a term pattern into a type pattern, accumulating bound variable names
promotePat :: DPat -> QWithAux VarPromotions PrM DType
promotePat (DLitPa lit) = promoteLit lit
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
promotePat DWildPa = do
  name <- qNewName "z"
  return $ DVarT name

promoteExp :: DExp -> PrM (DType, ADExp)
promoteExp (DVarE name) = fmap (, ADVarE name) $ lookupVarE name
promoteExp (DConE name) = return $ (promoteValRhs name, ADConE name)
promoteExp (DLitE lit)  = fmap (, ADLitE lit) $ promoteLit lit
promoteExp (DAppE exp1 exp2) = do
  (exp1', ann_exp1) <- promoteExp exp1
  (exp2', ann_exp2) <- promoteExp exp2
  return (apply exp1' exp2', ADAppE ann_exp1 ann_exp2)
promoteExp (DLamE names exp) = do
  lambdaName <- newUniqueName "Lambda"
  resultKVarName  <- qNewName "r"
  tyNames <- mapM mkTyName names
  let var_proms = zip names tyNames
  (rhs, ann_exp) <- lambdaBind var_proms $ promoteExp exp
  tyFamLamTypes <- mapM (const $ qNewName "t") names
  all_locals <- allLocals
  let all_args = all_locals ++ tyFamLamTypes
  tvbs <- mapM inferKindTV all_args
  let resultK       = DVarK resultKVarName
      m_resultK     = unknownResult resultK
  emitDecs [DClosedTypeFamilyD lambdaName
                               tvbs
                               m_resultK
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
  tvbs  <- mapM inferKindTV all_args
  resultK    <- fmap DVarK $ qNewName "r"
  emitDecs [DClosedTypeFamilyD caseTFName tvbs (unknownResult resultK) eqns]
  return ( prom_case `DAppT` exp'
         , ADCaseE ann_exp ann_matches )
promoteExp (DLetE decs exp) = do
  letPrefix <- fmap nameBase $ newUniqueName "Let"
  (binds, ann_env) <- promoteLetDecs letPrefix decs
  (exp', ann_exp) <- letBind binds $ promoteExp exp
  return (exp', ADLetE ann_env ann_exp)
promoteExp (DSigE exp ty) = do
  (exp', ann_exp) <- promoteExp exp
  ty' <- promoteType ty
  return (DSigT exp' ty', ADSigE ann_exp ty)

promoteLit :: Monad m => Lit -> m DType
promoteLit (IntegerL n)
  | n >= 0    = return $ DLitT (NumTyLit n)
  | otherwise = fail ("Promoting negative integers not supported: " ++ (show n))
promoteLit (StringL str) = return $ DLitT (StrTyLit str)
promoteLit lit =
  fail ("Only string and natural number literals can be promoted: " ++ show lit)

