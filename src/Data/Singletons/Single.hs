{- Data/Singletons/Single.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to refine constructs to work with singleton
types. It is an internal module to the singletons package.
-}
{-# LANGUAGE TemplateHaskell, CPP, TupleSections #-}

module Data.Singletons.Single where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (falseName, trueName, Quasi(..))
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons.Promote.Monad
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.LetDecEnv
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Sweeten
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Applicative
import Data.Foldable ( foldrM )
import Data.Maybe

singFamily :: DType
singFamily = DConT singFamilyName

singKindConstraint :: DKind -> DPred
singKindConstraint k = DAppPr (DConPr singKindClassName) (kindParam k)

demote :: DType
demote = DConT demoteRepName

-- | Generate singleton definitions from a type that is already defined.
-- For example, the singletons package itself uses
--
-- > $(genSingletons [''Bool, ''Maybe, ''Either, ''[]])
--
-- to generate singletons for Prelude types.
genSingletons :: Quasi q => [Name] -> q [Dec]
genSingletons names = do
  checkForRep names
  ddecs <- concatMapM (singInfo <=< dsInfo <=< reifyWithWarning) names
  return $ decsToTH ddecs

-- | Make promoted and singleton versions of all declarations given, retaining
-- the original declarations.
-- See <http://www.cis.upenn.edu/~eir/packages/singletons/README.html> for
-- further explanation.
singletons :: Quasi q => q [Dec] -> q [Dec]
singletons qdecs = do
  decs <- qdecs
  singDecs <- wrapDesugar singTopLevelDecs decs
  return (decs ++ singDecs)

-- | Make promoted and singleton versions of all declarations given, discarding
-- the original declarations.
singletonsOnly :: Quasi q => q [Dec] -> q [Dec]
singletonsOnly = (>>= wrapDesugar singTopLevelDecs)

-- | Create instances of 'SEq' and type-level '(:==)' for each type in the list
singEqInstances :: Quasi q => [Name] -> q [Dec]
singEqInstances = concatMapM singEqInstance

-- | Create instance of 'SEq' and type-level '(:==)' for the given type
singEqInstance :: Quasi q => Name -> q [Dec]
singEqInstance name = do
  promotion <- promoteEqInstance name
  dec <- singEqualityInstance sEqClassDesc name
  return $ dec ++ promotion

-- | Create instances of 'SEq' (only -- no instance for '(:==)', which 'SEq' generally
-- relies on) for each type in the list
singEqInstancesOnly :: Quasi q => [Name] -> q [Dec]
singEqInstancesOnly = concatMapM singEqInstanceOnly

-- | Create instances of 'SEq' (only -- no instance for '(:==)', which 'SEq' generally
-- relies on) for the given type
singEqInstanceOnly :: Quasi q => Name -> q [Dec]
singEqInstanceOnly name = singEqualityInstance sEqClassDesc name

-- | Create instances of 'SDecide' for each type in the list.
--
-- Note that, due to a bug in GHC 7.6.3 (and lower) optimizing instances
-- for SDecide can make GHC hang. You may want to put
-- @{-# OPTIONS_GHC -O0 #-}@ in your file.
singDecideInstances :: Quasi q => [Name] -> q [Dec]
singDecideInstances = concatMapM singDecideInstance

-- | Create instance of 'SDecide' for the given type.
--
-- Note that, due to a bug in GHC 7.6.3 (and lower) optimizing instances
-- for SDecide can make GHC hang. You may want to put
-- @{-# OPTIONS_GHC -O0 #-}@ in your file.
singDecideInstance :: Quasi q => Name -> q [Dec]
singDecideInstance name = singEqualityInstance sDecideClassDesc name

singInfo :: Quasi q => DInfo -> q [DDec]
singInfo (DTyConI dec Nothing) = do -- TODO: document this special case
  singTopLevelDecs [dec]
singInfo (DTyConI {}) =
  fail "Singling of things with instances not yet supported" -- TODO: fix
singInfo (DPrimTyConI _name _numArgs _unlifted) =
  fail "Singling of primitive type constructors not supported"
singInfo (DVarI _name _ty _mdec _fixity) =
  fail "Singling of value info not supported"
singInfo (DTyVarI _name _ty) =
  fail "Singling of type variable info not supported"

-- Note [Creating singleton functions in two stages]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Creating a singletons from declaration is conducted in two stages:
--
--  1) Singletonize everything except function bodies. When
--     singletonizing type signature for a function it might happen
--     that parameters that are functions will receive extra 'Proxy t'
--     arguments. We record information about number of Proxy
--     arguments required by each function argument.
--  2) Singletonize function bodies. We use information acquired in
--     step one to introduce extra Proxy arguments in the function body.
singTopLevelDecs :: Quasi q => [DDec] -> q [DDec]
singTopLevelDecs decls = do
  promDecls <- promoteMDecs $ promoteDecs decls
  singDecsM $ do
    let (letDecls, otherDecls) = partitionLetDecs decls
    (newLetDecls, newOtherDecls) <- singLetDecs TopLevel letDecls $
                                    concatMapM singDec otherDecls
    return $ promDecls ++ (map DLetDec newLetDecls) ++ newOtherDecls

singDec :: DDec -> SgM [DDec]
singDec (DLetDec dec) =
  fail $ "Internal error! LetDec encountered in singDec: " ++ show dec
singDec (DDataD _nd cxt name tvbs ctors derivings) =
  singDataD cxt name tvbs ctors derivings
singDec (DTySynD _name _tvbs _ty) =
  fail "Singling of type synonyms not yet supported"
singDec (DClassD _cxt _name _tvbs _fundeps _decs) =
  fail "Singling of class declaration not yet supported"
singDec (DInstanceD _cxt _ty _decs) =
  fail "Singling of class instance not yet supported"
singDec (DForeignD _fgn) =
  fail "Singling of foreign functions not supported"
singDec (DPragmaD _prag) = do
  qReportWarning "Singling of pragmas not supported"
  return []
singDec (DFamilyD _flavour _name _tvbs _mkind) =
  fail "Singling of type and data families not yet supported"
singDec (DDataInstD _nd _cxt _name _tys _ctors _derivings) =
  fail "Singling of data instances not yet supported"
singDec (DTySynInstD _name _eqns) =
  fail "Singling of type family instances not yet supported"
singDec (DClosedTypeFamilyD _name _tvs _mkind _eqns) =
  fail "Singling of closed type families not yet supported"
singDec (DRoleAnnotD _name _roles) =
  return [] -- silently ignore role annotations, as they're harmless

data TopLevelFlag = TopLevel | NotTopLevel

singLetDecs :: TopLevelFlag -> [DLetDec] -> SgM a -> SgM ([DLetDec], a)
singLetDecs top_level letDecls thing_inside = do
  let_dec_env <- buildLetDecEnv letDecls
  singLetDecEnv top_level let_dec_env $ thing_inside

singLetDecEnv :: TopLevelFlag -> LetDecEnv -> SgM a -> SgM ([DLetDec], a)
singLetDecEnv top_level
              (LetDecEnv { lde_defns = defns
                         , lde_types = types
                         , lde_infix = infix_decls })
              thing_inside = do
  (typeSigs, proxyTableEntries, m_promotions)
    <- mapAndUnzip3M (uncurry sing_ty_sig) (Map.toList types)
  let proxyTable   = Map.fromList proxyTableEntries
      infix_decls' = map (uncurry sing_infix_decl) infix_decls
  bindProxies proxyTable $ bindLets (catMaybes m_promotions) $ do
    let_decs <- mapM (uncurry sing_let_dec) (Map.toList defns)
    thing <- thing_inside
    return (infix_decls' ++ typeSigs ++ let_decs, thing)
  where
    sing_infix_decl :: Fixity -> Name -> DLetDec
    sing_infix_decl fixity name
      | isUpcase name =
        -- is it a tycon name or a datacon name??
        -- it *must* be a datacon name, because symbolic tycons
        -- can't be promoted. This is terrible.
        DInfixD fixity (singDataConName name)
      | otherwise = DInfixD fixity (singValName name)

    sing_ty_sig :: Name -> DType
                -> SgM ( DLetDec               -- the new type signature
                       , (Name, ProxySpec)     -- the proxy table entry
                       , Maybe (Name, DType))  -- the let-expansion entry
    sing_ty_sig name ty = do
      (tyTrans, proxies) <- singType ty
      let typeSig   = tyTrans (promoteValRhs name)
          sName     = singValName name
          promotion
            | TopLevel <- top_level
            = Nothing      -- promotion at top-level doesn't extend env't
            | otherwise
            = case typeSig of
                DAppT (DConT singFamilyName') tyarg
                  | singFamilyName' == singFamilyName
                  -> Just (name, tyarg)
                _ -> Just (name, error "Passing let-bound functions not supported")
      return (DSigD sName typeSig, (sName, proxies), promotion)

    sing_let_dec :: Name -> LetDecRHS -> SgM DLetDec
    sing_let_dec name (Value exp) =
      DValD (DVarPa (singValName name)) <$> singExp exp
    sing_let_dec name (Function clauses) = do
      proxySpec <- lookupProxy (singValName name)
      DFunD (singValName name) <$> mapM (singClause proxySpec) clauses

-- refine a constructor. the first parameter is the type variable that
-- the singleton GADT is parameterized by
singCtor :: DType -> DCon -> SgM DCon
 -- polymorphic constructors are handled just
 -- like monomorphic ones -- the polymorphism in
 -- the kind is automatic
singCtor a (DCon _tvbs cxt name fields)
  | not (null cxt)
  = fail "Singling of constrained constructors not yet supported"
  | otherwise
  = do
  let types = tysOfConFields fields
      sName = singDataConName name
      sCon = singDataCon name
      pCon = DConT name
  indexNames <- mapM (const $ qNewName "n") types
  let indices = map DVarT indexNames
  kinds <- mapM promoteType types
  args <- zipWithM buildArgType types indices
  let tvbs = zipWith DKindedTV indexNames kinds
      kindedIndices = zipWith DSigT indices kinds

  -- SingI instance
  emitDecs 
    [DInstanceD (map (DAppPr (DConPr singIName)) indices)
                (DAppT (DConT singIName)
                       (foldType pCon kindedIndices))
                [DLetDec $ DValD (DVarPa singMethName)
                       (foldExp sCon (map (const $ DVarE singMethName) types))]]

  return $ DCon tvbs
                [foldl DAppPr (DConPr equalityName) [a, foldType pCon indices]]
                sName
                (DNormalC $ map (NotStrict,) args)
  where buildArgType :: DType -> DType -> SgM DType
        buildArgType ty index = do
          (typeFn, _) <- singType ty
          return $ typeFn index

-- We wish to consider the promotion of "Rep" to be *
-- not a promoted data constructor.
singDataD :: DCxt -> Name -> [DTyVarBndr] -> [DCon] -> [Name] -> SgM [DDec]
singDataD cxt name tvbs ctors derivings
  | (_:_) <- cxt = fail "Singling of constrained datatypes is not supported"
  | otherwise    = do
  aName <- qNewName "z"
  let a = DVarT aName
  let tvbNames = map extractTvbName tvbs
  k <- promoteType (foldType (DConT name) (map DVarT tvbNames))
  ctors' <- mapM (singCtor a) ctors

  -- instance for SingKind
  fromSingClauses <- mapM mkFromSingClause ctors
  toSingClauses   <- mapM mkToSingClause ctors
  let singKindInst =
        DInstanceD (map (singKindConstraint . DVarK) tvbNames)
                   (DAppT (DConT singKindClassName)
                          (kindParam k))
                   [ DTySynInstD demoteRepName $ DTySynEqn
                      [kindParam k]
                      (foldType (DConT name)
                        (map (DAppT demote . kindParam . DVarK) tvbNames))
                   , DLetDec $ DFunD fromSingName (fromSingClauses `orIfEmpty` emptyMethod aName)
                   , DLetDec $ DFunD toSingName   (toSingClauses   `orIfEmpty` emptyMethod aName) ]

  -- SEq instance
  sEqInsts <- if elem eqName derivings
              then mapM (mkEqualityInstance k ctors') [sEqClassDesc, sDecideClassDesc]
              else return []

  -- e.g. type SNat (a :: Nat) = Sing a
  let kindedSynInst =
        DTySynD (singTyConName name)
                [DKindedTV aName k]
                (DAppT singFamily a)

  return $ (DDataInstD Data [] singFamilyName [DSigT a k] ctors' []) :
           kindedSynInst :
           singKindInst :
           sEqInsts
  where -- in the Rep case, the names of the constructors are in the wrong scope
        -- (they're types, not datacons), so we have to reinterpret them.
        mkConName :: Name -> SgM Name
        mkConName
          | nameBase name == nameBase repName = mkDataName . nameBase
          | otherwise                         = return

        mkFromSingClause :: DCon -> SgM DClause
        mkFromSingClause c = do
          let (cname, numArgs) = extractNameArgs c
          cname' <- mkConName cname
          varNames <- replicateM numArgs (qNewName "b")
          return $ DClause [DConPa (singDataConName cname) (map DVarPa varNames)]
                           (foldExp
                              (DConE cname')
                              (map (DAppE (DVarE fromSingName) . DVarE) varNames))

        mkToSingClause :: DCon -> SgM DClause
        mkToSingClause (DCon _tvbs _cxt cname fields) = do
          let types = tysOfConFields fields
          varNames  <- mapM (const $ qNewName "b") types
          svarNames <- mapM (const $ qNewName "c") types
          promoted  <- mapM promoteType types
          cname' <- mkConName cname
          let recursiveCalls = zipWith mkRecursiveCall varNames promoted
          return $
            DClause [DConPa cname' (map DVarPa varNames)]
                    (multiCase recursiveCalls
                               (map (DConPa someSingDataName . listify . DVarPa)
                                    svarNames)
                               (DAppE (DConE someSingDataName)
                                         (foldExp (DConE (singDataConName cname))
                                                  (map DVarE svarNames))))

        mkRecursiveCall :: Name -> DKind -> DExp
        mkRecursiveCall var_name ki =
          DSigE (DAppE (DVarE toSingName) (DVarE var_name))
                (DAppT (DConT someSingTypeName) (kindParam ki))

        emptyMethod :: Name -> [DClause]
        emptyMethod n = [DClause [DVarPa n] (DCaseE (DVarE n) emptyMatches)]

-- Note [Singletonizing type signature]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Proces of singletonizing a type signature is conducted in two steps:
--
--  1. Prepare a singletonized (but not defunctionalized) type
--     signature. The result is returned as a function that expects
--     one type parameter. That parameter is the name of a type-level
--     equivalent (ie. a type family) of a function being promoted.
--     This is done by singTypeRec. Most of the implementation is
--     straightforward. The most interesting part is the promotion of
--     arrows (ArrowT clause). When we reach an arrow we expect that
--     both its parameters are placed within the context (this is done
--     by AppT clause). We promote the type of first parameter to a
--     kind and introduce it via kind-annotated type variable in a
--     forall. At this point arguments that are functions are
--     converted to TyFun representation. This is important for
--     defunctionalization.
--
--  2. Lift out foralls: accumulate separate foralls at the beginning
--     of type signature. So this:
--
--      forall (a :: k). Proxy a -> forall (b :: [k]). Proxy b -> SList (a ': b)
--
--     becomes:
--
--      forall (a :: k) (b :: [k]). Proxy a -> Proxy b -> SList (a ': b)
--
--     This was originally a workaround for #8031 but later this was
--     used as a part of defunctionalization algorithm. Lifting
--     foralls produces new type signature and a list of type
--     variables that represent type level functions (TyFun kind).
--
--  3. Introduce Apply and Proxy. Using the list of type variables
--     that are type level functions (see step 2) we convert each
--     application of such variable into application of Apply type
--     family. Also, for each type variable that was converted to
--     Apply we introduce a Proxy parameter. For example this
--     signature:
--
--       sEither_ ::
--         forall (t1 :: TyFun k1 k3 -> *)
--                (t2 :: TyFun k2 k3 -> *)
--                (t3 :: Either k1 k2).
--                (forall (t4 :: k1). Sing t4 -> Sing (t1 t4))
--             -> (forall (t5 :: k2). Sing t5 -> Sing (t2 t5))
--             -> Sing t3 -> Sing (Either_ t1 t2 t3)
--
--     is converted to:
--
--       sEither_ ::
--         forall (t1 :: TyFun k1 k3 -> *)
--                (t2 :: TyFun k2 k3 -> *)
--                (t3 :: Either k1 k2).
--                (forall (t4 :: k1). Proxy t1 -> Sing t4 -> Sing (Apply t1 t4))
--             -> (forall (t5 :: k2). Proxy t2 -> Sing t5 -> Sing (Apply t2 t5))
--             -> Sing t3 -> Sing (Either_ t1 t2 t3)
--
--     Note that Proxy parameters were introduced only for arguments
--     that are functions. This will require us to add extra Proxy
--     arguments when calling these functions in the function body
--     (see Note [Creating singleton functions in two stages]).
--
--  4. Steps 2 and 3 are mutually recursive, ie. we introduce Apply and Proxy
--     for each parameter in the function signature we are singletonizing. Why?
--     Because a higher order function may accept parameters that are themselves
--     higher order functions:
--
--       foo :: ((a -> b) -> a -> b) -> (a -> b)  -> a -> b
--       foo f g a = f g a
--
--     Here 'foo' is a higher order function for which we must introduce Apply
--     and Proxy, but so is 'f'. Hence the mutually recursive calls between
--     introduceApplyAndProxy and introduceApplyAndProxyWorker. Singletonized
--     foo looks like this:
--
--       sFoo :: forall (k1 :: TyFun (TyFun a b -> *) (TyFun a b -> *) -> *)
--                      (k2 :: TyFun a b -> *)
--                      (k3 :: a).
--               (forall (t1 :: TyFun a b -> *).
--                       Proxy k1 ->
--                       (forall (t2 :: a). Proxy t1 -> Sing t2 -> Sing (Apply t1 t2))
--               -> forall (t3 :: a). Sing t3 -> Sing (Apply (Apply k1 t1) t3))
--               -> (forall (t4 :: a). Proxy k2 -> Sing t4 -> Sing (Apply k2 t4))
--               -> Sing k3 -> Sing (Foo k1 k2 k3)
--       sFoo f g a = (f Proxy g) a
--
--     Luckily for us the Proxies we introduce for the higher-order parameter
--     are not reflected in the body of sFoo - it is assumed that 'f' will
--     handle passing Proxy paramters to 'g' internally. This allows us to
--     discard the Proxy count returned by introduceApplyAndProxy in the body of
--     introduceApplyAndProxyWorker.


-- The return type of singType is:
--
--   Type -> (Type, [Int])
--
-- where first Type is the type that will be substituted in the
-- signature (see Note [Singletonizing type signature]). The result is
-- a tuple containing the final type signature with its proxy table.
singType :: DType -> SgM (DType -> DType, ProxySpec)
singType ty = do
  (sTypeFn, proxies) <- singTypeRec [] ty
  return (\inner_ty -> liftOutForalls (sTypeFn inner_ty), proxies)

-- Lifts all foralls to the top-level. This is a workaround
-- for bug #8031 on GHC.
liftOutForalls :: DType -> DType
liftOutForalls =
  go [] [] []
  where
    go tyvars cxt args (DForallT tyvars1 cxt1 t1)
      = go (reverse tyvars1 ++ tyvars) (reverse cxt1 ++ cxt) args t1
    go tyvars cxt args (DSigT t1 _kind)  -- ignore these kind annotations, which have to be *
      = go tyvars cxt args t1
    go tyvars cxt args (DAppT (DAppT DArrowT arg1) res1)
      = go tyvars cxt (arg1 : args) res1
    go [] [] args t1
      = mk_fun_ty (reverse args) t1
    go tyvars cxt args t1
      = DForallT (reverse tyvars) (reverse cxt) (mk_fun_ty (reverse args) t1)

    mk_fun_ty [] res = res
    mk_fun_ty (arg1:args) res = DAppT (DAppT DArrowT arg1) (mk_fun_ty args res)

-- the first parameter is the list of types the current type is applied to
singTypeRec :: [DType] -> DType -> SgM (DType -> DType, ProxySpec)
singTypeRec (_:_) (DForallT _ _ _) =
  fail "I thought this was impossible in Haskell. Email me at eir@cis.upenn.edu with your code if you see this message."
singTypeRec [] (DForallT _ [] ty) = -- Sing makes handling foralls automatic
  singTypeRec [] ty
singTypeRec ctx (DForallT _tvbs cxt innerty) = do
  cxt' <- singContext cxt
  (innerty', proxies) <- singTypeRec ctx innerty
  return (\ty -> DForallT [] cxt' (innerty' ty), proxies)
singTypeRec ctx (DAppT ty1 ty2) =
  singTypeRec (ty2 : ctx) ty1 -- recur with the ty2 in the applied context
singTypeRec _ctx (DSigT _ty _knd) =
  fail "Singling of types with explicit kinds not yet supported"
singTypeRec (_:_) (DVarT _) =
  fail "Singling of type variables of arrow kinds not yet supported"
singTypeRec [] (DVarT _name) =
  return $ (\ty -> DAppT singFamily ty, [])
singTypeRec _ctx (DConT _name) = -- we don't need to process the context with Sing
  return $ (\ty -> DAppT singFamily ty, [])
singTypeRec ctx DArrowT = case ctx of
  [ty1, ty2] -> do
    t               <- qNewName "t"
    (sty1, _)       <- singTypeRec [] ty1
    (sty2, proxies) <- singTypeRec [] ty2
    k1              <- promoteType ty1
    proxy_flag      <- needs_proxy ty1
    return ( \f -> DForallT [DKindedTV t k1]
                            []
                            (maybe_add_proxy proxy_flag (DVarT t) $
                             DArrowT `DAppT` (sty1 (DVarT t))
                                     `DAppT` (sty2 $ DConT applyName
                                                      `DAppT` f
                                                      `DAppT` (DVarT t)))
           , proxy_flag : proxies )
    where
      needs_proxy :: DType -> SgM ProxyFlag
      needs_proxy (DForallT {}) =
        fail "Cannot singletonize a higher-rank type."
      needs_proxy (DAppT (DAppT DArrowT _) _) = return YesProxy
      needs_proxy (DSigT ty _) = needs_proxy ty
      needs_proxy _ = return NoProxy

      maybe_add_proxy :: ProxyFlag  -- add the proxy?
                      -> DType      -- proxy for this type
                      -> DType      -- added to this type
                      -> DType      -- creates this type
      maybe_add_proxy NoProxy  _   ty = ty
      maybe_add_proxy YesProxy pty ty =
        DAppT (DAppT DArrowT (DAppT (DConT proxyTypeName) pty)) ty
  _ -> fail "Internal error in Sing: converting ArrowT with improper context"
singTypeRec _ctx (DLitT _) = fail "Singling of type-level literals not yet supported"

-- refine a constraint context
singContext :: DCxt -> SgM DCxt
singContext = mapM singPred

singPred :: DPred -> SgM DPred
singPred = singPredRec []

singPredRec :: [DType] -> DPred -> SgM DPred
singPredRec ctx (DAppPr pr ty) = singPredRec (ty : ctx) pr
singPredRec _ctx (DSigPr _pr _ki) =
  fail "Singling of constraints with explicit kinds not yet supported"
singPredRec _ctx (DVarPr _n) =
  fail "Singling of contraint variables not yet supported"
singPredRec ctx (DConPr n)
  | n == equalityName
  = fail "Singling of type equality constraints not yet supported"
  | otherwise = do
    kis <- mapM promoteType ctx
    let sName = singClassName n
    return $ foldl DAppPr (DConPr sName) (map kindParam kis)

singClause :: ProxySpec -> DClause -> SgM DClause
singClause proxyCount (DClause pats exp) = do
  sPats <- mapM (singPat Parameter) pats
  sPats' <- foldrM insert_proxies [] $ zip proxyCount sPats
  sBody <- singExp exp
  return $ DClause sPats' sBody
  where
    insert_proxies :: (ProxyFlag, DPat) -> [DPat] -> SgM [DPat]
    insert_proxies (NoProxy, p)        ps = return (p : ps)
    insert_proxies (YesProxy, p)       ps = do
      proxy_name <- qNewName "_proxy"
      return (DVarPa proxy_name : p : ps)

-- we need to know where a pattern is to anticipate when
-- GHC's brain might explode
data PatternContext = LetBinding
                    | CaseStatement
                    | Parameter
                    deriving Eq

checkIfBrainWillExplode :: Monad m => PatternContext -> m ()
checkIfBrainWillExplode CaseStatement = return ()
checkIfBrainWillExplode Parameter = return ()
checkIfBrainWillExplode _ =
  fail $ "Can't use a singleton pattern outside of a case-statement or\n" ++
         "do expression: GHC's brain will explode if you try. (Do try it!)"

singPat :: PatternContext -> DPat -> SgM DPat
singPat _patCxt (DLitPa _lit) =
  fail "Singling of literal patterns not yet supported"
singPat _patCxt (DVarPa name) = return $ DVarPa (singValName name)
singPat patCxt (DConPa name pats) = do
  checkIfBrainWillExplode patCxt
  pats' <- mapM (singPat patCxt) pats
  return $ DConPa (singDataConName name) pats'
singPat patCxt (DTildePa pat) = do
  pat' <- singPat patCxt pat
  return $ DTildePa pat'
singPat patCxt (DBangPa pat) = do
  pat' <- singPat patCxt pat
  return $ DBangPa pat'
singPat _patCxt DWildPa = return DWildPa
singPat _patCxt p@(DSigPa {}) =
  fail $ "This should be impossible -- GHC doesn't quote scoped type variables, "
      ++ "and th-desugar doesn't desugar them!\n"
      ++ show p

singExp :: DExp -> SgM DExp
singExp = singExpRec []

singExpRec :: [DExp]    -- not yet sing'ed
           -> DExp -> SgM DExp
singExpRec []   (DVarE name) = return (singVal name)
singExpRec args (DVarE name) = do
  proxies <- lookupProxy (singValName name)
  args'  <- foldrM insert_proxies [] $ zip proxies args
  return (foldExp (singVal name) args')
  where
    insert_proxies :: (ProxyFlag, DExp) -> [DExp] -> SgM [DExp]
    insert_proxies (NoProxy,  e) es = (:) <$> singExp e <*> pure es
    insert_proxies (YesProxy, e) es = do
      ty <- liftPrM $ promoteExp e
      e' <- singExp e
      return (proxyFor ty : e' : es)
singExpRec args (DConE name) = foldExp (singDataCon name) <$> mapM singExp args
singExpRec args (DLitE lit) = foldExp <$> (singLit lit) <*> mapM singExp args
singExpRec args (DAppE exp1 exp2) = singExpRec (exp2 : args) exp1
singExpRec args (DLamE names exp) = do
  -- DLamE doesn't allow type signatures in the patterns, so we have to
  -- use a nested Case construct. Tiresome, but not bad.
  let sNames = map singValName names
      pats = map DVarPa sNames
  arg_names <- mapM (const $ qNewName "arg") names
  exp' <- singExp exp
  foldExp (DLamE arg_names (DCaseE (mkTupleDExp $ map DVarE arg_names)
                                   [DMatch (mkTupleDPat pats) exp']))
          <$> mapM singExp args
singExpRec args (DCaseE exp matches) = do
  exp' <- singExp exp
  matches' <- mapM singMatch matches
  foldExp (DCaseE exp' matches') <$> mapM singExp args
singExpRec args (DLetE decs exp) = do
  (decs', exp') <- singLetDecs NotTopLevel decs $ singExp exp
  foldExp (DLetE decs' exp') <$> mapM singExp args
singExpRec args (DSigE exp ty) = do
  exp' <- singExp exp
  (ty', _) <- singType ty
  pty <- liftPrM $ promoteExp exp
  foldExp (DSigE exp' (ty' pty)) <$> mapM singExp args

singMatch :: DMatch -> SgM DMatch
singMatch (DMatch pat exp) = do
  sPat <- singPat CaseStatement pat
  sExp <- singExp exp
  return $ DMatch sPat sExp

singLit :: Lit -> SgM DExp
singLit lit = DSigE (DVarE singMethName) <$> (DAppT singFamily <$> (promoteLit lit))

kindParam :: DKind -> DType
kindParam k = DSigT (DConT kProxyDataName) (DConK kProxyTypeName [k])

proxyFor :: DType -> DExp
proxyFor ty = DSigE (DConE proxyDataName) (DAppT (DConT proxyTypeName) ty)

singDataCon :: Name -> DExp
singDataCon = DConE . singDataConName

singVal :: Name -> DExp
singVal = DVarE . singValName

-----------------------------------------------------
-- Generating SEq and SDecide instances
-----------------------------------------------------

-- generalized function for creating equality instances
singEqualityInstance :: Quasi q => EqualityClassDesc q -> Name -> q [Dec]
singEqualityInstance desc@(_, className, _) name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of " ++
                            show className ++ " for it.") name
  dtvbs <- mapM dsTvb tvbs
  dcons <- mapM dsCon cons
  let tyvars = map (DVarK . extractTvbName) dtvbs
      kind = DConK name tyvars
  aName <- qNewName "a"
  let aVar = DVarT aName
  (scons, _) <- singM $ mapM (singCtor aVar) dcons
  eqInstance <- mkEqualityInstance kind scons desc
  return $ decToTH eqInstance

-- making the SEq instance and the SDecide instance are rather similar,
-- so we generalize
type EqualityClassDesc q = ((DCon, DCon) -> q DClause, Name, Name)
sEqClassDesc, sDecideClassDesc :: Quasi q => EqualityClassDesc q
sEqClassDesc = (mkEqMethClause, sEqClassName, sEqMethName)
sDecideClassDesc = (mkDecideMethClause, sDecideClassName, sDecideMethName)

-- pass the *singleton* constructors, not the originals
mkEqualityInstance :: Quasi q => DKind -> [DCon]
                   -> EqualityClassDesc q -> q DDec
mkEqualityInstance k ctors (mkMeth, className, methName) = do
  let ctorPairs = [ (c1, c2) | c1 <- ctors, c2 <- ctors ]
  methClauses <- if null ctors
                 then mkEmptyMethClauses
                 else mapM mkMeth ctorPairs
  return $ DInstanceD (map (\kvar -> (DConPr className) `DAppPr` kindParam kvar)
                           (getKindVars k))
                     (DAppT (DConT className)
                            (kindParam k))
                     [DLetDec $ DFunD methName methClauses]
  where getKindVars :: DKind -> [DKind]
        getKindVars (DVarK x)         = [DVarK x]
        getKindVars (DConK _ args)    = concatMap getKindVars args
        getKindVars DStarK            = []
        getKindVars (DArrowK arg res) = concatMap getKindVars [arg, res]
        getKindVars other             =
          error ("getKindVars sees an unusual kind: " ++ show other)

        mkEmptyMethClauses :: Quasi q => q [DClause]
        mkEmptyMethClauses = do
          a <- qNewName "a"
          return [DClause [DVarPa a, DWildPa] (DCaseE (DVarE a) emptyMatches)]

mkEqMethClause :: Quasi q => (DCon, DCon) -> q DClause
mkEqMethClause (c1, c2)
  | lname == rname = do
    lnames <- replicateM lNumArgs (qNewName "a")
    rnames <- replicateM lNumArgs (qNewName "b")
    let lpats = map DVarPa lnames
        rpats = map DVarPa rnames
        lvars = map DVarE lnames
        rvars = map DVarE rnames
    return $ DClause
      [DConPa lname lpats, DConPa rname rpats]
      (allExp (zipWith (\l r -> foldExp (DVarE sEqMethName) [l, r])
                        lvars rvars))
  | otherwise =
    return $ DClause
      [DConPa lname (replicate lNumArgs DWildPa),
       DConPa rname (replicate rNumArgs DWildPa)]
      (singDataCon falseName)
  where allExp :: [DExp] -> DExp
        allExp [] = singDataCon trueName
        allExp [one] = one
        allExp (h:t) = DAppE (DAppE (singVal andName) h) (allExp t)

        (lname, lNumArgs) = extractNameArgs c1
        (rname, rNumArgs) = extractNameArgs c2

mkDecideMethClause :: Quasi q => (DCon, DCon) -> q DClause
mkDecideMethClause (c1, c2)
  | lname == rname =
    if lNumArgs == 0
    then return $ DClause [DConPa lname [], DConPa rname []]
                          (DAppE (DConE provedName) (DConE reflName))
    else do
      lnames <- replicateM lNumArgs (qNewName "a")
      rnames <- replicateM lNumArgs (qNewName "b")
      contra <- qNewName "contra"
      let lpats = map DVarPa lnames
          rpats = map DVarPa rnames
          lvars = map DVarE lnames
          rvars = map DVarE rnames
      refl <- qNewName "refl"
      return $ DClause
        [DConPa lname lpats, DConPa rname rpats]
        (DCaseE (mkTupleDExp $
                 zipWith (\l r -> foldExp (DVarE sDecideMethName) [l, r])
                         lvars rvars)
                ((DMatch (mkTupleDPat (replicate lNumArgs
                                        (DConPa provedName [DConPa reflName []])))
                        (DAppE (DConE provedName) (DConE reflName))) :
                 [DMatch (mkTupleDPat (replicate i DWildPa ++
                                       DConPa disprovedName [DVarPa contra] :
                                       replicate (lNumArgs - i - 1) DWildPa))
                         (DAppE (DConE disprovedName)
                                (DLamE [refl] $
                                 DCaseE (DVarE refl)
                                        [DMatch (DConPa reflName []) $
                                         (DAppE (DVarE contra)
                                                (DConE reflName))]))
                 | i <- [0..lNumArgs-1] ]))

  | otherwise = do
    x <- qNewName "x"
    return $ DClause
      [DConPa lname (replicate lNumArgs DWildPa),
       DConPa rname (replicate rNumArgs DWildPa)]
      (DAppE (DConE disprovedName) (DLamE [x] (DCaseE (DVarE x) emptyMatches)))

  where
    (lname, lNumArgs) = extractNameArgs c1
    (rname, rNumArgs) = extractNameArgs c2
