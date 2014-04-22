{- Data/Singletons/Single.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to refine constructs to work with singleton
types. It is an internal module to the singletons package.
-}
{-# LANGUAGE TemplateHaskell, CPP, TupleSections, ParallelListComp #-}

module Data.Singletons.Single where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (falseName, trueName, Quasi(..))
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons.Promote.Monad ( promoteMDecs, promoteM )
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.LetDecEnv
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Sweeten
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import Control.Monad
import Control.Applicative

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
  let (letDecls, otherDecls) = partitionLetDecs decls
  otherDecls' <- promoteMDecs $ promoteNonLetDecs otherDecls
  ((_, letDecEnv), letDecls') <- promoteM $ promoteLetDecs noPrefix letDecls
  singDecsM $ do
    let letBinds = concatMap buildLets otherDecls
    (newLetDecls, newOtherDecls) <- bindLets letBinds $
                                    singLetDecEnv TopLevel letDecEnv $
                                    concatMapM singDec otherDecls
    return $ otherDecls' ++ letDecls' ++ (map DLetDec newLetDecls) ++ newOtherDecls

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

buildLets :: DDec -> [(Name, DExp)]
buildLets (DDataD _nd _cxt _name _tvbs cons _derivings) =
  concatMap con_num_args cons
  where
    con_num_args :: DCon -> [(Name, DExp)]
    con_num_args (DCon _tvbs _cxt name fields) =
      (name, wrapSingFun (length (tysOfConFields fields))
                         (promoteValRhs name) (DConE $ singDataConName name))
      : rec_selectors fields

    rec_selectors :: DConFields -> [(Name, DExp)]
    rec_selectors (DNormalC {}) = []
    rec_selectors (DRecC fields) =
      let names = map fstOf3 fields in
      [ (name, wrapSingFun 1 (promoteValRhs name) (DVarE $ singValName name))
      | name <- names ]
      
buildLets (DClassD _cxt _name _tvbs _fds decs) = concatMap buildLets decs
buildLets _ = []

data TopLevelFlag = TopLevel | NotTopLevel

singLetDecEnv :: TopLevelFlag -> ALetDecEnv -> SgM a -> SgM ([DLetDec], a)
singLetDecEnv top_level
              (LetDecEnv { lde_defns = defns
                         , lde_types = types
                         , lde_infix = infix_decls
                         , lde_proms = proms })
              thing_inside = do
  (typeSigs, letBinds, tyvarNames)
    <- mapAndUnzip3M (uncurry sing_ty_sig) (Map.toList proms)
  let infix_decls' = map (uncurry sing_infix_decl) infix_decls
  bindLets letBinds $ do
    let_decs <- mapM (uncurry (sing_let_dec (Map.fromList tyvarNames))) (Map.toList defns)
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

    sing_ty_sig :: Name -> DType   -- the type is the promoted type, not the type sig!
                -> SgM ( DLetDec               -- the new type signature
                       , (Name, DExp)          -- the let-bind entry
                       , (Name, [Name])        -- the scoped tyvar names in the tysig
                       ) 
    sing_ty_sig name prom_ty =
      let sName = singValName name in
      case Map.lookup name types of
        Nothing -> do
          num_args <- guess_num_args name
          (sty, tyvar_names) <- mk_sing_ty num_args prom_ty
          return ( DSigD sName sty
                 , (name, wrapSingFun num_args prom_ty (DVarE sName))
                 , (name, tyvar_names) )
        Just ty -> do
          (sty, num_args, tyvar_names) <- singType top_level prom_ty ty
          return ( DSigD sName sty
                 , (name, wrapSingFun num_args prom_ty (DVarE sName))
                 , (name, tyvar_names) )

    guess_num_args :: Name -> SgM Int
    guess_num_args name =
      case Map.lookup name defns of
        Nothing -> fail "Internal error: promotion known for something not let-bound."
        Just (AValue n _) -> return n
        Just (AFunction _ n _) -> return n

    mk_sing_ty :: Int -> DType -> SgM (DType, [Name])
    mk_sing_ty n prom_ty = do
      arg_names <- replicateM n (qNewName "arg")
      return ( DForallT (map DPlainTV arg_names) []
                        (ravel (map (\name -> singFamily `DAppT` DVarT name) arg_names
                                ++ [singFamily `DAppT`
                                    (foldl apply prom_ty (map DVarT arg_names))]))
             , arg_names )

    sing_let_dec :: Map Name [Name] -> Name -> ALetDecRHS -> SgM DLetDec
    sing_let_dec _bound_names name (AValue num_arrows exp) =
      DValD (DVarPa (singValName name)) <$>
      (wrapUnSingFun num_arrows <$> singExp exp)
    sing_let_dec bound_names name (AFunction prom_fun num_arrows clauses) =
      let tyvar_names = case Map.lookup name bound_names of
                          Nothing -> []
                          Just ns -> ns
      in
      DFunD (singValName name) <$> mapM (singClause prom_fun num_arrows tyvar_names) clauses

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
      sCon = DConE sName
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

  let conFields = case fields of
                    DNormalC _ -> DNormalC $ map (NotStrict,) args
                    DRecC rec_fields ->
                      DRecC [ (singValName field_name, NotStrict, arg)
                            | (field_name, _, _) <- rec_fields
                            | arg <- args ]
  return $ DCon tvbs
                [foldl DAppPr (DConPr equalityName) [a, foldType pCon indices]]
                sName
                conFields
  where buildArgType :: DType -> DType -> SgM DType
        buildArgType ty index = do
          (ty', _, _) <- singType NotTopLevel index ty
          return ty'

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
singType :: TopLevelFlag -> DType -> DType -> SgM (DType, Int, [Name])
singType top_level prom ty = do
  let (cxt, tys) = unravel ty
      args       = init tys
      num_args   = length args
  cxt' <- mapM singPred cxt
  arg_names <- replicateM num_args (qNewName "t")
  let args' = map (\n -> singFamily `DAppT` (DVarT n)) arg_names
      res'  = singFamily `DAppT` (foldl apply prom (map DVarT arg_names))
      tau   = ravel (args' ++ [res'])
  ty' <- case top_level of
           TopLevel -> do
             prom_args <- mapM promoteType args
             return $ DForallT (zipWith DKindedTV arg_names prom_args)
                               cxt' tau
                -- don't annotate kinds. Why? Because the original source
                -- may have used scoped type variables, and we're just
                -- not clever enough to get the scoped kind variables right.
                -- (the business in bindTyVars gets in the way)
                -- If ScopedTypeVariables was actually sane in patterns,
                -- this restriction might be able to be lifted.
           NotTopLevel -> return $ DForallT (map DPlainTV arg_names)
                                            cxt' tau
  return (ty', num_args, arg_names)

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

singClause :: DType   -- the promoted function
           -> Int     -- the number of arrows in the type. If this is more
                      -- than the number of patterns, we need to eta-expand
                      -- with unSingFun.
           -> [Name]  -- the names of the forall'd vars in the type sig of this
                      -- function. This list should have the same length as the
                      -- number of patterns in the clause
           -> ADClause -> SgM DClause
singClause prom_fun num_arrows bound_names (ADClause var_proms pats exp) = do
  ((sPats, prom_pats), wilds)
    <- evalForPair $ mapAndUnzipM (singPat (Map.fromList var_proms) Parameter) pats
  let equalities = zip (map DVarT bound_names) prom_pats
  sBody <- bindTyVarsClause var_proms wilds
                            (foldl apply prom_fun prom_pats) equalities $ singExp exp
  let sBody' = wrapUnSingFun (num_arrows - length pats) sBody
  return $ DClause sPats sBody'

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

singPat :: Map Name Name   -- from term-level names to type-level names
        -> PatternContext -> DPat -> QWithAux [Name]  -- these names must be forall-bound
                                     SgM ( DPat
                                         , DType ) -- the type form of the pat
singPat _var_proms _patCxt (DLitPa _lit) =
  fail "Singling of literal patterns not yet supported"
singPat var_proms _patCxt (DVarPa name) = do
  tyname <- case Map.lookup name var_proms of
              Nothing     -> qNewName (nameBase name)
              Just tyname -> return tyname
  return (DVarPa (singValName name), DVarT tyname)
singPat var_proms patCxt (DConPa name pats) = do
  checkIfBrainWillExplode patCxt
  (pats', tys) <- mapAndUnzipM (singPat var_proms patCxt) pats
  return ( DConPa (singDataConName name) pats'
         , foldl apply (promoteValRhs name) tys )
singPat var_proms patCxt (DTildePa pat) = do
  (pat', ty) <- singPat var_proms patCxt pat
  return (DTildePa pat', ty)
singPat var_proms patCxt (DBangPa pat) = do
  (pat', ty) <- singPat var_proms patCxt pat
  return (DBangPa pat', ty)
singPat _var_proms _patCxt DWildPa = do
  wild <- qNewName "wild"
  addElement wild
  return (DWildPa, DVarT wild)

singExp :: ADExp -> SgM DExp
singExp (ADVarE name)  = lookupVarE name
singExp (ADConE name)  = lookupConE name
singExp (ADLitE lit)   = singLit lit
singExp (ADAppE e1 e2) = do
  e1' <- singExp e1
  e2' <- singExp e2
  return $ (DVarE applySingName) `DAppE` e1' `DAppE` e2'
singExp (ADLamE var_proms prom_lam names exp) = do
  let sNames = map singValName names
  exp' <- bindTyVars var_proms [] (foldl apply prom_lam (map (DVarT . snd) var_proms)) $
          singExp exp
  return $ wrapSingFun (length names) prom_lam $ DLamE sNames exp'
singExp (ADCaseE exp matches) = DCaseE <$> singExp exp <*> mapM singMatch matches
singExp (ADLetE env exp) =
  uncurry DLetE <$> singLetDecEnv NotTopLevel env (singExp exp)
singExp (ADSigE {}) =
  fail "Singling of explicit type annotations not yet supported."

singMatch :: ADMatch -> SgM DMatch
singMatch (ADMatch var_proms prom_match pat exp) = do
  ((sPat, prom_pat), wilds)
    <- evalForPair $ singPat (Map.fromList var_proms) CaseStatement pat
        -- why DAppT below? See comment near decl of ADMatch in LetDecEnv.
  sExp <- bindTyVars var_proms wilds (prom_match `DAppT` prom_pat) $ singExp exp
  return $ DMatch sPat sExp

singLit :: Lit -> SgM DExp
singLit lit = DSigE (DVarE singMethName) <$> (DAppT singFamily <$> (promoteLit lit))

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
      (DConE $ singDataConName falseName)
  where allExp :: [DExp] -> DExp
        allExp [] = DConE $ singDataConName trueName
        allExp [one] = one
        allExp (h:t) = DAppE (DAppE (DVarE $ singValName andName) h) (allExp t)

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
