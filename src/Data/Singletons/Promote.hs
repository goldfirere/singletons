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
import Data.Singletons.Promote.Equality
import Data.Singletons.Util
import Data.Singletons.LetDecEnv
import Prelude hiding (exp)
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.Map.Strict as Map

-- | Promote every declaration given to the type level, retaining the originals.
promote :: Quasi q => q [Dec] -> q [Dec]
promote qdec = do
  decls <- qdec
  ddecls <- dsDecs decls
  promDecls <- promoteMDecs $ promoteDecs ddecls
  return $ decls ++ decsToTH promDecls

-- | Promote each declaration, discarding the originals.
promoteOnly :: Quasi q => q [Dec] -> q [Dec]
promoteOnly qdec = do
  decls  <- qdec
  ddecls <- dsDecs decls
  promDecls <- promoteMDecs $ promoteDecs ddecls
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

promoteInfo :: DInfo -> PrM [DDec]
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
-- Promoting declarations proceeds in two stages:
-- 1) Promote everything except type signatures.
--    Function and value declarations don't get promoted yet, but the key
--    information (either the function clauses or the value RHS) get stored
--    in a LetDecEnv.

-- 2) Promote type signatures. This must be done in a second pass
--    because a function type signature gets promoted to a type family
--    declaration.  Although function signatures do not differentiate
--    between uniform parameters and non-uniform parameters, type
--    family declarations do. We need to process a function's
--    definition to get the count of non-uniform parameters before
--    producing the type family declaration.  At this point, any
--    function written without a type signature is rejected and
--    removed.
--
-- Consider this example:
--
--   foo :: Int -> Bool -> Bool
--   foo 0 = id
--   foo _ = not
--
-- Here the first parameter to foo is non-uniform, because it is
-- inspected in a pattern and can be different in each defining
-- equation of foo. The second parameter to foo, specified in the type
-- signature as Bool, is a uniform parameter - it is not inspected and
-- each defining equation of foo uses it the same way. The foo
-- function will be promoted to a type familty Foo like this:
--
--   type family Foo (n :: Int) :: Bool -> Bool where
--      Foo 0 = Id
--      Foo a = Not
--
-- To generate type signature for Foo type family we must first learn
-- what is the actual number of patterns used in defining cequations
-- of foo. In this case there is only one so we declare Foo to take
-- one argument and have return type of Bool -> Bool.

-- Promote a list of top-level declarations.
promoteDecs :: [DDec] -> PrM [DDec]
promoteDecs decls = do
  checkForRepInDecls decls
  -- See Note [Promoting declarations in two stages]
  let (let_decs, other_decs) = partitionLetDecs decls
  
    -- promoteLetDecs returns LetBinds, which we don't need at top level
  _ <- promoteLetDecs noPrefix let_decs
  promoteNonLetDecs other_decs

promoteNonLetDecs :: [DDec] -> PrM [DDec]
promoteNonLetDecs other_decs = do
  rec_selectors <- concatMapM extract_rec_selectors other_decs
  _ <- promoteLetDecs noPrefix rec_selectors
  concatMapM promoteDec other_decs
  where
    extract_rec_selectors :: DDec -> PrM [DLetDec]
    extract_rec_selectors (DDataD _nd _cxt data_name tvbs cons _derivings) =
      let arg_ty = foldType (DConT data_name)
                            (map (DVarT . extractTvbName) tvbs)
      in
      concatMapM (getRecordSelectors arg_ty) cons
    extract_rec_selectors _ = return []
    
  
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

{---------------------------------------------------------------------

Note [Why lhsToDecs works]
~~~~~~~~~~~~~~~~~~~~~~~~~~

It seems odd (to RAE, at least) that a *symbol* type synonym is identical
to a normal one. Won't the kind of a normal type synonym not be
defunctionalized? Ah, but it would be, because the RHS is promoted in
a defunctionalized way. So, it all works out.

---------------------------------------------------------------------}

promoteDec :: DDec -> PrM [DDec]
promoteDec (DLetDec letdec) = fail $ "Internal error! Let-declarations should " ++
                                     "not be seen in promoteDec: " ++ show letdec

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
promoteDec (DDataD _nd _cxt name tvbs ctors derivings) = do
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
  return []  -- promoting the datatype leads to no decs directly...
  
promoteDec (DTySynD _name _tvbs _ty) =
  fail "Promotion of type synonym declaration not yet supported"
promoteDec (DClassD _cxt _name _tvbs _fundeps _decs) =
  fail "Promotion of class declaration not yet supported"
promoteDec (DInstanceD _cxt _ty _decs) =
  fail "Promotion of instance declaration not yet supported"
promoteDec (DForeignD _fgn) =
  fail "Promotion of foreign function declaration not yet supported"
promoteDec (DPragmaD _prag) =
  fail "Promotion of pragmas not yet supported"
promoteDec (DFamilyD _flavour _name _tvbs _mkind) =
  fail "Promotion of type and data families not yet supported"
promoteDec (DDataInstD _nd _cxt _name _tys _ctors _derivings) =
  fail "Promotion of data instances not yet supported"
promoteDec (DTySynInstD _name _eqn) =
  fail "Promotion of type synonym instances not yet supported"
promoteDec (DClosedTypeFamilyD _name _tvs _mkind _eqns) =
  fail "Promotion of closed type families not yet supported"
promoteDec (DRoleAnnotD _name _roles) =
  return [] -- silently ignore role annotations, as they're harmless here

promoteLetDecEnv :: String -> ULetDecEnv -> PrM ([DDec], ALetDecEnv)
promoteLetDecEnv prefix (LetDecEnv { lde_defns = value_env
                                   , lde_types = type_env
                                   , lde_infix = infix_decls }) = do
  let infix_decls'  = catMaybes $ map (uncurry prom_infix_decl) infix_decls
      (names, rhss) = unzip $ Map.toList value_env
  (decs, ann_rhss) <- fmap unzip $ zipWithM prom_let_dec names rhss
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
             , AValue num_arrows ann_exp )
    prom_let_dec name (UFunction clauses) = do
      numArgs <- count_args clauses
      (m_argKs, m_resK, ty_num_args) <- case Map.lookup name type_env of
#if __GLASGOW_HASKELL__ < 707
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

-- the only monadic thing we do here is fail. This allows the function
-- to be used from the Singletons module
promoteType :: Monad m => DType -> m DKind
promoteType = go []
  where
    go :: Monad m => [DKind] -> DType -> m DKind
    -- We don't need to worry about constraints: they are used to express
    -- static guarantees at runtime. But, because we don't need to do
    -- anything special to keep static guarantees at compile time, we don't
    -- need to promote them.
    go []       (DForallT _tvbs _cxt ty) = go [] ty
    go []       (DAppT (DAppT DArrowT (DForallT (_:_) _ _)) _) =
      fail "Cannot promote types of rank above 1."
    go args     (DAppT t1 t2) = do
      k2 <- go [] t2
      go (k2 : args) t1
    go args     (DSigT ty _) = go args ty  -- just ignore signatures
    go []       (DVarT name) = return $ DVarK name
    go _        (DVarT name) = fail $ "Cannot promote an applied type variable " ++
                                      show name ++ "."
    go []       (DConT name)
      | name == typeRepName               = return DStarK
      | name == stringName                = return $ DConK symbolName []
      | nameBase name == nameBase repName = return DStarK
    go args     (DConT name)
      | Just n <- unboxedTupleNameDegree_maybe name
      = return $ DConK (tupleTypeName n) args
      | otherwise
      = return $ DConK name args
    go [k1, k2] DArrowT = return $ addStar (DConK tyFunName [k1, k2])
    go _ (DLitT _) = fail "Cannot promote a type-level literal"

    go args     hd = fail $ "Illegal Haskell construct encountered:\n" ++
                            "headed by: " ++ show hd ++ "\n" ++
                            "applied to: " ++ show args

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

---------------------------------------------------------------------
-- Defunctionalization
---------------------------------------------------------------------

defunInfo :: DInfo -> PrM [DDec]
defunInfo (DTyConI dec _instances) = buildDefunSyms dec
defunInfo (DPrimTyConI _name _numArgs _unlifted) =
  fail $ "Building defunctionalization symbols of primitive " ++
         "type constructors not supported"
defunInfo (DVarI _name _ty _mdec _fixity) =
  fail "Building defunctionalization symbols of values not supported"
defunInfo (DTyVarI _name _ty) =
  fail "Building defunctionalization symbols of type variables not supported"

buildDefunSyms :: DDec -> PrM [DDec]
buildDefunSyms (DDataD _new_or_data _cxt tyName tvbs ctors _derivings) =
  buildDefunSymsDataD tyName tvbs ctors
buildDefunSyms (DClosedTypeFamilyD name tvbs returnK_maybe _) = do
  let arg_m_kinds = map extractTvbKind tvbs
  defunctionalize name arg_m_kinds returnK_maybe
buildDefunSyms (DFamilyD TypeFam name tvbs returnK_maybe) = do
  let arg_kinds = map (default_to_star . extractTvbKind) tvbs
      res_kind  = default_to_star returnK_maybe
      default_to_star Nothing  = Just DStarK
      default_to_star (Just k) = Just k
  defunctionalize name arg_kinds res_kind
buildDefunSyms _ = fail $ "Defunctionalization symbols can only be built for " ++
                          "type families and data declarations"

buildDefunSymsDataD :: Name -> [DTyVarBndr] -> [DCon] -> PrM [DDec]
buildDefunSymsDataD tyName tvbs ctors = do
  let kis = map (DVarK . extractTvbName) tvbs
      promotedKind = DConK tyName kis
  concatMapM (promoteCtor promotedKind) ctors
  where
    promoteCtor :: DKind -> DCon -> PrM [DDec]
    promoteCtor promotedKind ctor = do
      let (name, arg_tys) = extractNameTypes ctor
      arg_kis <- mapM promoteType arg_tys
      defunctionalize name (map Just arg_kis) (Just promotedKind)

-- Generate data declarations and apply instances
-- required for defunctionalization.
-- For a type family:
--
-- type family Foo (m :: Nat) (n :: Nat) (l :: Nat) :: Nat
--
-- we generate data declarations that allow us to talk about partial
-- application at the type level:
--
-- type FooSym3 a b c = Foo a b c
-- data FooSym2 :: Nat -> Nat -> (TyFun Nat Nat) -> *
-- type instance Apply (FooSym2 a b) c = FooSym3 a b c
-- data FooSym1 :: Nat -> (TyFun Nat (TyFun Nat Nat -> *)) -> *
-- type instance Apply (FooSym1 a) b = FooSym2 a b
-- data FooSym0 :: TyFun Nat (TyFun Nat (TyFun Nat Nat -> *) -> *) -> *
-- type instance Apply FooSym0 a = FooSym1 a
--
-- defunctionalize takes list of kinds of the type family parameters and
-- kind of the result.
defunctionalize :: Name -> [Maybe DKind] -> Maybe DKind -> PrM [DDec]
defunctionalize name m_arg_kinds m_res_kind = do
  let num_args = length m_arg_kinds
      sat_name = promoteTySym name num_args
  tvbNames <- replicateM num_args $ qNewName "t"
  let sat_dec = DTySynD sat_name (zipWith mk_tvb tvbNames m_arg_kinds)
                        (foldType (DConT name) (map DVarT tvbNames))
  other_decs <- go (num_args - 1) (reverse m_arg_kinds) m_res_kind
  return $ sat_dec : other_decs
  where
    mk_tvb :: Name -> Maybe DKind -> DTyVarBndr
    mk_tvb tvb_name Nothing  = DPlainTV tvb_name
    mk_tvb tvb_name (Just k) = DKindedTV tvb_name k
    
    go :: Int -> [Maybe DKind] -> Maybe DKind -> PrM [DDec]
    go _ [] _ = return []
    go n (m_arg : m_args) m_result = do
      decls <- go (n - 1) m_args (addStar_maybe (buildTyFun_maybe m_arg m_result))
      fst_name : rest_names <- replicateM (n + 1) (qNewName "l")
      extra_name <- qNewName "arg"
      let data_name   = promoteTySym name n
          next_name   = promoteTySym name (n+1)
          con_name    = mkName $ nameBase (data_name) ++ "KindInference"
          m_tyfun     = buildTyFun_maybe m_arg m_result
          arg_params  = zipWith mk_tvb rest_names (reverse m_args)
          tyfun_param = mk_tvb fst_name m_tyfun
          arg_names   = map extractTvbName arg_params
          params      = arg_params ++ [tyfun_param]
          con_eq_ct   = foldl DAppPr (DConPr equalityName)
                          [ foldType (DConT data_name) (map DVarT arg_names)
                            `apply`
                            (DVarT extra_name)
                          , foldType (DConT next_name) (map DVarT (arg_names ++ [extra_name]))
                          ]
          con_decl    = DCon [DPlainTV extra_name]
                             [con_eq_ct]
                             con_name
                             (DNormalC [( NotStrict
                                       , DConT proxyTypeName `DAppT`
                                         DVarT extra_name )])
          data_decl   = DDataD Data [] data_name params [con_decl] []
          app_eqn     = DTySynEqn [ foldType (DConT data_name)
                                             (map DVarT rest_names)
                                  , DVarT fst_name ]
                                  (foldType (DConT (promoteTySym name (n+1)))
                                            (map DVarT (rest_names ++ [fst_name])))
          app_decl    = DTySynInstD applyName app_eqn
          suppress    = DInstanceD [] (DConT suppressClassName `DAppT` DConT data_name)
                          [DLetDec $ DFunD suppressMethodName
                                           [DClause [DWildPa]
                                                    ((DVarE 'snd) `DAppE`
                                                     mkTupleDExp [DConE con_name,
                                                                  mkTupleDExp []])]]
      return $ suppress : data_decl : app_decl : decls

buildTyFun :: DKind -> DKind -> DKind
buildTyFun k1 k2 = DConK tyFunName [k1, k2]
    
buildTyFun_maybe :: Maybe DKind -> Maybe DKind -> Maybe DKind
buildTyFun_maybe m_k1 m_k2 = do
  k1 <- m_k1
  k2 <- m_k2
  return $ DConK tyFunName [k1, k2]

addStar :: DKind -> DKind
addStar t = DArrowK t DStarK

addStar_maybe :: Maybe DKind -> Maybe DKind
addStar_maybe t = DArrowK <$> t <*> pure DStarK

-- Counts the arity of type level function represented with TyFun constructors
-- TODO: this and isTuFun looks like a good place to use PatternSynonyms
tyFunArity :: DKind -> Int
tyFunArity (DArrowK (DConK tyFunNm [_, b]) DStarK)
  | tyFunName == tyFunNm
  = 1 + tyFunArity b
tyFunArity _ = 0

-- Checks if type is (TyFun a b -> *)
isTyFun :: DKind -> Bool
isTyFun (DArrowK (DConK tyFunNm [_,_]) DStarK)
  | tyFunName == tyFunNm
  = True
isTyFun _ = False

-- Build TyFun kind from the list of kinds
ravelTyFun :: [DKind] -> DKind
ravelTyFun []    = error "Internal error: TyFun raveling nil"
ravelTyFun [k]   = k
ravelTyFun kinds = go tailK (buildTyFun k2 k1)
    where (k1 : k2 : tailK) = reverse kinds
          go []     acc = addStar acc
          go (k:ks) acc = go ks (buildTyFun k (addStar acc))
