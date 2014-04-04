{- Data/Singletons/Promote.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to promote term-level constructs to the
type level. It is an internal module to the singletons package.
-}

{-# LANGUAGE TemplateHaskell, CPP, MultiWayIf, LambdaCase, TupleSections #-}

module Data.Singletons.Promote where

import Language.Haskell.TH hiding ( Q, cxt )
import Language.Haskell.TH.Syntax ( falseName, trueName, Quasi(..) )
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Sweeten
import Data.Singletons.Names
import Data.Singletons.Promote.Monad
import Data.Singletons.Promote.Util
import Data.Singletons.Promote.Equality
import Data.Singletons.Util
import Data.Singletons.Types
import Data.Singletons
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)
import Prelude hiding (exp)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ( Map )
import Data.Set ( Set )
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Typeable ( TypeRep )

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
  let (let_decs, other_decs) = partitionWith (\case DLetDec ld -> Left ld
                                                    dec        -> Right dec) decls
      -- promoteLetDecs returns LetBinds, which we don't need
  _ <- promoteLetDecs noPrefix let_decs
  prom_decs <- concatMapM promoteDec other_decs
  return prom_decs

promoteLetDecs :: String -- prefix to use on all new definitions
               -> [DLetDec] -> PrM [LetBind]
promoteLetDecs prefix decls = do
  let_dec_env <- concatMapM promoteLetDec decls
  all_locals <- allLocals
  let binds = [ (name, foldType (DConT sym) (map DVarT all_locals))
              | name <- Map.keys $ fst let_dec_env
              , let proName = promoteValNameLhsPrefix prefix name
                    sym = promoteTySym proName (length all_locals) ]
  emitDecsM $ letBind binds $ promoteLetDecEnv prefix let_dec_env
  return binds

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

promoteLetDec :: DLetDec -> PrM LetDecEnv
promoteLetDec (DFunD name clauses) = do
  valueBinding name (Function clauses)
promoteLetDec (DValD (DVarPa name) exp) = do
  valueBinding name (Value exp)
promoteLetDec dec@(DValD {}) = do  -- complex pattern. Flatten!
  let_decs <- flattenDValD dec
  concatMapM promoteLetDec let_decs
promoteLetDec (DSigD name ty) = do
  typeBinding name ty
promoteLetDec (DInfixD fixity name)
  | isUpcase name = return emptyLetDecEnv -- promoting a type or data ctor
  | otherwise     = do                    -- value
    emitDecs [DLetDec $ DInfixD fixity (promoteValNameLhs name)]
    return emptyLetDecEnv

promoteLetDecEnv :: String -> LetDecEnv -> PrM [DDec]
promoteLetDecEnv prefix (value_env, type_env) =
  mapM (uncurry prom_let_dec) (Map.toList value_env)
  where
    prom_let_dec :: Name -> LetDecRHS -> PrM DDec
    prom_let_dec name (Value exp) = do
      all_locals <- allLocals
      exp' <- promoteExp exp
      let proName = promote_lhs name
      (_, kinds) <- mapAndUnzipM inferKindTV all_locals
      (res_kind, mk_rhs)
        <- case Map.lookup name type_env of
             Nothing -> (, id) <$> (fmap DVarK $ qNewName "kresult")
             Just ty -> do
               ki <- promoteType ty
               return (ki, (`DSigT` ki))
      emitDecsM $ defunctionalize proName kinds res_kind
      return (DTySynD proName (map DPlainTV all_locals) (mk_rhs exp'))
    prom_let_dec name (Function clauses) =
      case Map.lookup name type_env of
        Nothing -> fail ("No type signature for function \"" ++
                         (nameBase name) ++ "\". Cannot promote.")
        Just ty -> do
            -- promoteType turns arrows into TyFun. So, we unravel first to
            -- avoid this behavior. Note the use of ravelTyFun in resultK
            -- to make the return kind work out
          kis <- mapM promoteType (unravel ty)
          numArgs <- count_args clauses
          let proName = promote_lhs name
              (argKs, resultKs) = splitAt numArgs kis
              resultK = ravelTyFun resultKs
          all_locals <- allLocals
          (local_tvbs, local_kis) <- mapAndUnzipM inferKindTV all_locals
          tyvarNames <- mapM (const $ qNewName "a") argKs
          let all_args = local_tvbs ++ zipWith DKindedTV tyvarNames argKs
          emitDecsM $ defunctionalize proName (local_kis ++ argKs) resultK
          eqns <- mapM promoteClause clauses
          return (DClosedTypeFamilyD proName all_args (Just resultK) eqns)

    promote_lhs = promoteValNameLhsPrefix prefix

    count_args (DClause pats _ : _) = return $ length pats
    count_args _ = fail $ "Impossible! A function without clauses."

promoteType :: DType -> PrM DKind
promoteType = go []
  where
    go :: [DKind] -> DType -> PrM DKind
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
    go [k1, k2] DArrowT = return $ addStar (buildTyFun k1 k2)
    go _ (DLitT _) = fail "Cannot promote a type-level literal"

    go args     head = fail $ "Illegal Haskell construct encountered:\n" ++
                              "headed by: " ++ show head ++ "\n" ++
                              "applied to: " ++ show args

{-
-- Takes a name of a function together with its arity and
-- converts all normal applications of that variable into usage of
-- Apply type family. Returns a type and a Bool that says whether
-- Apply was introduced or not. This extra flag is used when
-- singletonizing type signature.

-- The algorithm here is actually a bit tricky because it has to deal
-- with cases when function has arity larger than 1. For example this
-- body:
--
--   f a b
--
-- must be promoted to
--
--  Apply (Apply f a) b
--
-- Algorithm can be viewed as having two passes:
--
--  1. A top-down pass recurses to the bottom of abstract syntax tree
--     and inserts first application of Apply whenever it encounters a
--     variable that represents a function. So in the above example
--     upon reaching application (f a) it would generate (Apply f a)
--     and recurse on b. This is done by the second case of "go"
--     helper function.
--
--  2. Inserting Apply triggers a bottom-up pass that adds additional
--     Applys if required by function arity. Second case of "go"
--     helper function inspects third parameter returned by recursive
--     call to go for ty1. If it is greater than 0 it means we have to
--     add additional Apply.
introduceApply :: (Name, Int) -> DType -> (DType, Bool)
introduceApply funData' typeT =
    case go funData' typeT of
      (t, f, _ ) -> (t, f)
    where
      go funData (DForallT tyvars ctx ty) =
          let (ty', isApplyAdded, n) = go funData ty
          in (DForallT tyvars ctx ty', isApplyAdded, n)
      go (funName, funArity) (DAppT (DVarT name) ty) =
          let (ty', isApplyAdded, _) = go (funName, funArity) ty
          in if funName == name -- if we found our type variable then use Apply
             then (DAppT (DAppT (DConT applyName) (DVarT name)) ty'
                  , True, funArity - 1)
             else (DAppT                        (DVarT name)  ty'
                  , isApplyAdded, 0)
      go funData (DAppT ty1 ty2) =
          let (ty1', isApplyAdded1, n) = go funData ty1
              (ty2', isApplyAdded2, _) = go funData ty2
          in if n /= 0 -- do we need to insert more Applies because arity > 1?
             then (DAppT (DAppT (DConT applyName) ty1') ty2', True  , n - 1)
             else (DAppT ty1' ty2', isApplyAdded1 || isApplyAdded2, n    )
      go _ ty = (ty, False, 0)
-}

promoteClause :: DClause -> PrM DTySynEqn
promoteClause (DClause pats exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  (types, new_vars) <- evalForPair $ mapM promotePat pats
  ty <- lambdaBind new_vars $ promoteExp exp
  all_locals <- allLocals   -- these are bound *outside* of this clause
  return $ DTySynEqn (map DVarT all_locals ++ types) ty

promoteMatch :: DMatch -> PrM DTySynEqn
promoteMatch (DMatch pat exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  (ty, new_vars) <- evalForPair $ promotePat pat
  rhs <- lambdaBind new_vars $ promoteExp exp
  all_locals <- allLocals
  return $ DTySynEqn (map DVarT all_locals ++ [ty]) rhs

-- promotes a term pattern into a type pattern, accumulating bound variable names
promotePat :: DPat -> QWithAux [Name] PrM DType
promotePat (DLitPa lit) = promoteLit lit
promotePat (DVarPa name) = do
    -- somewhat cheekily use the same name at the type level
  addElement name
  return $ DVarT name
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

promoteExp :: DExp -> PrM DType
promoteExp (DVarE name) = lookupVarE name
promoteExp (DConE name) = return $ promoteValRhs name
promoteExp (DLitE lit)  = promoteLit lit
promoteExp (DAppE exp1 exp2) = apply <$> promoteExp exp1 <*> promoteExp exp2
promoteExp (DLamE names exp) = do
  lambdaName <- newUniqueName "Lambda"
  resultKVarName  <- qNewName "r"
  rhs <- lambdaBind names $ promoteExp exp
  tyFamLamTypes <- mapM (const $ qNewName "t") names
  all_locals <- allLocals
  let all_args = all_locals ++ tyFamLamTypes
  (tvbs, kinds) <- mapAndUnzipM inferKindTV all_args
  let resultK       = DVarK resultKVarName
      m_resultK     = unknownResult resultK
  emitDecs [DClosedTypeFamilyD lambdaName
                               tvbs
                               m_resultK
                               [DTySynEqn (map DVarT (all_locals ++ names))
                                          rhs]]
  emitDecsM $ defunctionalize lambdaName kinds resultK
  return $ foldl apply (DConT (promoteTySym lambdaName 0)) (map DVarT all_locals)
promoteExp (DCaseE exp matches) = do
  caseTFName <- newUniqueName "Case"
  exp'       <- promoteExp exp
  eqns       <- mapM promoteMatch matches
  tyvarName  <- qNewName "t"
  all_locals <- allLocals
  let all_args = all_locals ++ [tyvarName]
  (tvbs, _)  <- mapAndUnzipM inferKindTV all_args
  resultK    <- fmap DVarK $ qNewName "r"
  emitDecs [DClosedTypeFamilyD caseTFName tvbs (unknownResult resultK) eqns]
  return $ foldType (DConT caseTFName) (map DVarT all_locals ++ [exp'])
promoteExp (DLetE decs exp) = do
  letPrefix <- fmap nameBase $ newUniqueName "Let"
  binds <- promoteLetDecs letPrefix decs
  letBind binds $ promoteExp exp
promoteExp (DSigE exp ty) = DSigT <$> promoteExp exp <*> promoteType ty

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
  arg_kinds <- mapM (inferKind . extractTvbKind) tvbs
  res_kind  <- inferKind returnK_maybe
  defunctionalize name arg_kinds res_kind
buildDefunSyms (DFamilyD TypeFam name tvbs returnK_maybe) = do
  let arg_kinds = map (default_to_star . extractTvbKind) tvbs
      res_kind  = default_to_star returnK_maybe
      default_to_star Nothing  = DStarK
      default_to_star (Just k) = k
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
      defunctionalize name arg_kis promotedKind

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
defunctionalize :: Name -> [DKind] -> DKind -> PrM [DDec]
defunctionalize name args result = do
  let num_args = length args
      sat_name = promoteTySym name num_args
  tvbNames <- replicateM num_args $ qNewName "t"
     -- use kind inference on the saturated definition. This causes no harm
     -- in 7.6.3 but is crucial in 7.8, when sometimes the args are too polymorphic
  let sat_dec = DTySynD sat_name (map DPlainTV tvbNames)
                        (foldType (DConT name) (map DVarT tvbNames))
  other_decs <- go (num_args - 1) (reverse args) result
  return $ sat_dec : other_decs
  where
    go :: Int -> [DKind] -> DKind -> PrM [DDec]
    go _ [] _ = return []
    go n (arg : args) result = do
      decls <- go (n - 1) args (addStar (buildTyFun arg result))
      tvbNames@(fst_name : rest_names) <- replicateM (n + 1) (qNewName "l")
      let data_name = promoteTySym name n
          tyfun     = buildTyFun arg result
          params    = zipWith DKindedTV tvbNames (reverse (tyfun : args))
          data_decl = DDataD Data [] data_name params [] []
          app_eqn   = DTySynEqn [ foldType (DConT data_name)
                                           (map DVarT rest_names)
                                , DVarT fst_name ]
                                (foldType (DConT (promoteTySym name (n+1)))
                                          (map DVarT (rest_names ++ [fst_name])))
          app_decl  = DTySynInstD applyName app_eqn
      return $ data_decl : app_decl : decls
    
buildTyFun :: DKind -> DKind -> DKind
buildTyFun k1 k2 = DConK tyFunName [k1, k2]

addStar :: DKind -> DKind
addStar t = DArrowK t DStarK

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
ravelTyFun kinds = go tailK (buildTyFun k1 k2)
    where (k1 : k2 : tailK) = reverse kinds
          go []     acc = addStar acc
          go (k:ks) acc = go ks (buildTyFun k (addStar acc))
