{- Data/Singletons/Singletons.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to refine constructs to work with singleton
types. It is an internal module to the singletons package.
-}
{-# LANGUAGE TemplateHaskell, CPP, TupleSections #-}

module Data.Singletons.Singletons where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (falseName, trueName, Quasi(..))
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons
import Data.Singletons.Decide
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import Data.Singletons.Types

-- map to track bound variables
type ExpTable = Map.Map Name Exp

-- translating a type gives a type with a hole in it,
-- represented here as a function
type TypeFn = Type -> Type

-- a list of argument types extracted from a type application
type TypeContext = [Type]

singFamilyName, singIName, singMethName, demoteRepName, singKindClassName,
  sEqClassName, sEqMethName, sconsName, snilName, sIfName, undefinedName,
  kProxyDataName, kProxyTypeName, someSingTypeName, someSingDataName,
  nilName, consName, sListName, eqName, sDecideClassName, sDecideMethName,
  provedName, disprovedName, reflName, toSingName, fromSingName, listName :: Name
singFamilyName = ''Sing
singIName = ''SingI
singMethName = 'sing
toSingName = 'toSing
fromSingName = 'fromSing
demoteRepName = ''DemoteRep
singKindClassName = ''SingKind
sEqClassName = mkName "SEq"
sEqMethName = mkName "%:=="
sIfName = mkName "sIf"
undefinedName = 'undefined
sconsName = mkName "SCons"
snilName = mkName "SNil"
kProxyDataName = 'KProxy
kProxyTypeName = ''KProxy
someSingTypeName = ''SomeSing
someSingDataName = 'SomeSing
nilName = '[]
consName = '(:)
listName = ''[]
sListName = mkName "SList"
eqName = ''Eq
sDecideClassName = ''SDecide
sDecideMethName = '(%~)
provedName = 'Proved
disprovedName = 'Disproved
reflName = 'Refl

mkTupleName :: Int -> Name
mkTupleName n = mkName $ "STuple" ++ (show n)

singFamily :: Type
singFamily = ConT singFamilyName

singKindConstraint :: Kind -> Pred
singKindConstraint k = ClassP singKindClassName [kindParam k]

demote :: Type
demote = ConT demoteRepName

singDataConName :: Name -> Name
singDataConName nm
  | nm == nilName                           = snilName
  | nm == consName                          = sconsName
  | Just degree <- tupleNameDegree_maybe nm = mkTupleName degree
  | otherwise                               = prefixUCName "S" ":%" nm

singTyConName :: Name -> Name
singTyConName name
  | name == listName                          = sListName
  | Just degree <- tupleNameDegree_maybe name = mkTupleName degree
  | otherwise                                 = prefixUCName "S" ":%" name

singClassName :: Name -> Name
singClassName = singTyConName

singDataCon :: Name -> Exp
singDataCon = ConE . singDataConName

singValName :: Name -> Name
singValName n
  | nameBase n == "undefined" = undefinedName
  | otherwise                 = (prefixLCName "s" "%") $ upcase n

singVal :: Name -> Exp
singVal = VarE . singValName

kindParam :: Kind -> Type
kindParam k = SigT (ConT kProxyDataName) (AppT (ConT kProxyTypeName) k)

-- | Generate singleton definitions from a type that is already defined.
-- For example, the singletons package itself uses
--
-- > $(genSingletons [''Bool, ''Maybe, ''Either, ''[]])
--
-- to generate singletons for Prelude types.
genSingletons :: Quasi q => [Name] -> q [Dec]
genSingletons names = do
  checkForRep names
  concatMapM (singInfo <=< reifyWithWarning) names

singInfo :: Quasi q => Info -> q [Dec]
singInfo (ClassI _dec _instances) =
  fail "Singling of class info not supported"
singInfo (ClassOpI _name _ty _className _fixity) =
  fail "Singling of class members info not supported"
singInfo (TyConI dec) = singDec dec
singInfo (FamilyI _dec _instances) =
  fail "Singling of type family info not yet supported" -- KindFams
singInfo (PrimTyConI _name _numArgs _unlifted) =
  fail "Singling of primitive type constructors not supported"
singInfo (DataConI _name _ty _tyname _fixity) =
  fail $ "Singling of individual constructors not supported; " ++
         "single the type instead"
singInfo (VarI _name _ty _mdec _fixity) =
  fail "Singling of value info not supported"
singInfo (TyVarI _name _ty) =
  fail "Singling of type variable info not supported"

-- refine a constructor. the first parameter is the type variable that
-- the singleton GADT is parameterized by
-- runs in the QWithDecs monad because auxiliary declarations are produced
singCtor :: Quasi q => Type -> Con -> QWithDecs q Con
singCtor a = ctorCases
  -- monomorphic case
  (\name types -> do
    let sName = singDataConName name
        sCon = singDataCon name
        pCon = PromotedT name
    indexNames <- replicateM (length types) (qNewName "n")
    let indices = map VarT indexNames
    kinds <- mapM promoteType types
    args <- buildArgTypes types indices
    let tvbs = zipWith KindedTV indexNames kinds
        kindedIndices = zipWith SigT indices kinds

    -- SingI instance
    addElement $ InstanceD (map (ClassP singIName . listify) indices)
                           (AppT (ConT singIName)
                                 (foldType pCon kindedIndices))
                           [ValD (VarP singMethName)
                                 (NormalB $ foldExp sCon (replicate (length types)
                                                           (VarE singMethName)))
                                 []]

    return $ ForallC tvbs
                     [EqualP a (foldType pCon indices)]
                     (NormalC sName $ map (NotStrict,) args))

  -- polymorphic case
  (\_tvbs cxt ctor -> case cxt of
    _:_ -> fail "Singling of constrained constructors not yet supported"
    [] -> singCtor a ctor) -- polymorphic constructors are handled just
                           -- like monomorphic ones -- the polymorphism in
                           -- the kind is automatic
  where buildArgTypes :: Quasi q => [Type] -> [Type] -> q [Type]
        buildArgTypes types indices = do
          typeFns <- mapM singType types
          return $ zipWith id typeFns indices

-- | Make promoted and singleton versions of all declarations given, retaining
-- the original declarations.
-- See <http://www.cis.upenn.edu/~eir/packages/singletons/README.html> for
-- further explanation.
singletons :: Quasi q => q [Dec] -> q [Dec]
singletons = (>>= singDecs True)

-- | Make promoted and singleton versions of all declarations given, discarding
-- the original declarations.
singletonsOnly :: Quasi q => q [Dec] -> q [Dec]
singletonsOnly = (>>= singDecs False)

-- first parameter says whether or not to include original decls
singDecs :: Quasi q => Bool -> [Dec] -> q [Dec]
singDecs originals decls = do
  promDecls <- promoteDecs decls
  newDecls <- mapM singDec decls
  return $ (if originals then (decls ++) else id) $ promDecls ++ (concat newDecls)

singDec :: Quasi q => Dec -> q [Dec]
singDec (FunD name clauses) = do
  let sName = singValName name
      vars = Map.singleton name (VarE sName)
  listify <$> FunD sName <$> (mapM (singClause vars) clauses)
singDec (ValD _ (GuardedB _) _) =
  fail "Singling of definitions of values with a pattern guard not yet supported"
singDec (ValD _ _ (_:_)) =
  fail "Singling of definitions of values with a <<where>> clause not yet supported"
singDec (ValD pat (NormalB exp) []) = do
  (sPat, vartbl) <- evalForPair $ singPat TopLevel pat
  sExp <- singExp vartbl exp
  return [ValD sPat (NormalB sExp) []]
singDec (DataD cxt name tvbs ctors derivings) =
  singDataD False cxt name tvbs ctors derivings
singDec (NewtypeD cxt name tvbs ctor derivings) =
  singDataD False cxt name tvbs [ctor] derivings
singDec (TySynD _name _tvbs _ty) =
  fail "Singling of type synonyms not yet supported"
singDec (ClassD _cxt _name _tvbs _fundeps _decs) =
  fail "Singling of class declaration not yet supported"
singDec (InstanceD _cxt _ty _decs) =
  fail "Singling of class instance not yet supported"
singDec (SigD name ty) = do
  tyTrans <- singType ty
  return [SigD (singValName name) (tyTrans (promoteVal name))]
singDec (ForeignD fgn) =
  let name = extractName fgn in do
    qReportWarning $ "Singling of foreign functions not supported -- " ++
                    (show name) ++ " ignored"
    return []
  where extractName :: Foreign -> Name
        extractName (ImportF _ _ _ n _) = n
        extractName (ExportF _ _ n _) = n
singDec (InfixD fixity name)
  | isUpcase name = return [InfixD fixity (singDataConName name)]
  | otherwise     = return [InfixD fixity (singValName name)]
singDec (PragmaD _prag) = do
    qReportWarning "Singling of pragmas not supported"
    return []
singDec (FamilyD _flavour _name _tvbs _mkind) =
  fail "Singling of type and data families not yet supported"
singDec (DataInstD _cxt _name _tys _ctors _derivings) =
  fail "Singling of data instances not yet supported"
singDec (NewtypeInstD _cxt _name _tys _ctor _derivings) =
  fail "Singling of newtype instances not yet supported"
#if __GLASGOW_HASKELL__ >= 707
singDec (RoleAnnotD _name _roles) =
  return [] -- silently ignore role annotations, as they're harmless
singDec (ClosedTypeFamilyD _name _tvs _mkind _eqns) =
  fail "Singling of closed type families not yet supported"
singDec (TySynInstD _name _eqns) =
#else
singDec (TySynInstD _name _lhs _rhs) =
#endif
  fail "Singling of type family instances not yet supported"

-- | Create instances of 'SEq' and type-level '(:==)' for each type in the list
singEqInstances :: Quasi q => [Name] -> q [Dec]
singEqInstances = concatMapM singEqInstance

-- | Create instance of 'SEq' and type-level '(:==)' for the given type
singEqInstance :: Quasi q => Name -> q [Dec]
singEqInstance name = do
  promotion <- promoteEqInstance name
  dec <- singEqualityInstance sEqClassDesc name
  return $ dec : promotion

-- | Create instances of 'SEq' (only -- no instance for '(:==)', which 'SEq' generally
-- relies on) for each type in the list
singEqInstancesOnly :: Quasi q => [Name] -> q [Dec]
singEqInstancesOnly = concatMapM singEqInstanceOnly

-- | Create instances of 'SEq' (only -- no instance for '(:==)', which 'SEq' generally
-- relies on) for the given type
singEqInstanceOnly :: Quasi q => Name -> q [Dec]
singEqInstanceOnly name = listify <$> singEqualityInstance sEqClassDesc name

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
singDecideInstance name = listify <$> singEqualityInstance sDecideClassDesc name

-- generalized function for creating equality instances
singEqualityInstance :: Quasi q => EqualityClassDesc q -> Name -> q Dec
singEqualityInstance desc@(_, className, _) name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of " ++
                            show className ++ " for it.") name
  let tyvars = map (VarT . extractTvbName) tvbs
      kind = foldType (ConT name) tyvars
  aName <- qNewName "a"
  let aVar = VarT aName
  scons <- mapM (evalWithoutAux . singCtor aVar) cons
  mkEqualityInstance kind scons desc

-- making the SEq instance and the SDecide instance are rather similar,
-- so we generalize
type EqualityClassDesc q = ((Con, Con) -> q Clause, Name, Name)
sEqClassDesc, sDecideClassDesc :: Quasi q => EqualityClassDesc q
sEqClassDesc = (mkEqMethClause, sEqClassName, sEqMethName)
sDecideClassDesc = (mkDecideMethClause, sDecideClassName, sDecideMethName)

-- pass the *singleton* constructors, not the originals
mkEqualityInstance :: Quasi q => Kind -> [Con]
                   -> EqualityClassDesc q -> q Dec
mkEqualityInstance k ctors (mkMeth, className, methName) = do
  let ctorPairs = [ (c1, c2) | c1 <- ctors, c2 <- ctors ]
  methClauses <- if null ctors
                 then mkEmptyMethClauses
                 else mapM mkMeth ctorPairs
  return $ InstanceD (map (\kvar -> ClassP className [kindParam kvar])
                          (getKindVars k))
                     (AppT (ConT className)
                           (kindParam k))
                     [FunD methName methClauses]
  where getKindVars :: Kind -> [Kind]
        getKindVars (AppT l r) = getKindVars l ++ getKindVars r
        getKindVars (VarT x)   = [VarT x]
        getKindVars (ConT _)   = []
        getKindVars StarT      = []
        getKindVars other      =
          error ("getKindVars sees an unusual kind: " ++ show other)

        mkEmptyMethClauses :: Quasi q => q [Clause]
        mkEmptyMethClauses = do
          a <- qNewName "a"
          return [Clause [VarP a, WildP] (NormalB (CaseE (VarE a) emptyMatches)) []]

mkEqMethClause :: Quasi q => (Con, Con) -> q Clause
mkEqMethClause (c1, c2)
  | lname == rname = do
    lnames <- replicateM lNumArgs (qNewName "a")
    rnames <- replicateM lNumArgs (qNewName "b")
    let lpats = map VarP lnames
        rpats = map VarP rnames
        lvars = map VarE lnames
        rvars = map VarE rnames
    return $ Clause
      [ConP lname lpats, ConP rname rpats]
      (NormalB $
        allExp (zipWith (\l r -> foldExp (VarE sEqMethName) [l, r])
                        lvars rvars))
      []
  | otherwise =
    return $ Clause
      [ConP lname (replicate lNumArgs WildP),
       ConP rname (replicate rNumArgs WildP)]
      (NormalB (singDataCon falseName))
      []
  where allExp :: [Exp] -> Exp
        allExp [] = singDataCon trueName
        allExp [one] = one
        allExp (h:t) = AppE (AppE (singVal andName) h) (allExp t)

        (lname, lNumArgs) = extractNameArgs c1
        (rname, rNumArgs) = extractNameArgs c2

mkDecideMethClause :: Quasi q => (Con, Con) -> q Clause
mkDecideMethClause (c1, c2)
  | lname == rname =
    if lNumArgs == 0
    then return $ Clause [ConP lname [], ConP rname []]
                         (NormalB (AppE (ConE provedName) (ConE reflName))) []
    else do
      lnames <- replicateM lNumArgs (qNewName "a")
      rnames <- replicateM lNumArgs (qNewName "b")
      contra <- qNewName "contra"
      let lpats = map VarP lnames
          rpats = map VarP rnames
          lvars = map VarE lnames
          rvars = map VarE rnames
      return $ Clause
        [ConP lname lpats, ConP rname rpats]
        (NormalB $
         CaseE (mkTupleExp $
                zipWith (\l r -> foldExp (VarE sDecideMethName) [l, r])
                        lvars rvars)
               ((Match (mkTuplePat (replicate lNumArgs
                                      (ConP provedName [ConP reflName []])))
                       (NormalB $ AppE (ConE provedName) (ConE reflName))
                      []) :
                [Match (mkTuplePat (replicate i WildP ++
                                    ConP disprovedName [VarP contra] :
                                    replicate (lNumArgs - i - 1) WildP))
                       (NormalB $ AppE (ConE disprovedName)
                                       (LamE [ConP reflName []]
                                             (AppE (VarE contra)
                                                   (ConE reflName))))
                       [] | i <- [0..lNumArgs-1] ]))
        []

  | otherwise =
    return $ Clause
      [ConP lname (replicate lNumArgs WildP),
       ConP rname (replicate rNumArgs WildP)]
      (NormalB (AppE (ConE disprovedName) (LamCaseE emptyMatches)))
      []

  where
    (lname, lNumArgs) = extractNameArgs c1
    (rname, rNumArgs) = extractNameArgs c2

-- the first parameter is True when we're refining the special case "Rep"
-- and false otherwise. We wish to consider the promotion of "Rep" to be *
-- not a promoted data constructor.
singDataD :: Quasi q => Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> [Name] -> q [Dec]
singDataD rep cxt name tvbs ctors derivings
  | (_:_) <- cxt = fail "Singling of constrained datatypes is not supported"
  | otherwise    = do
  aName <- qNewName "z"
  let a = VarT aName
  let tvbNames = map extractTvbName tvbs
  k <- promoteType (foldType (ConT name) (map VarT tvbNames))
  (ctors', ctorInstDecls) <- evalForPair $ mapM (singCtor a) ctors

  -- instance for SingKind
  fromSingClauses <- mapM mkFromSingClause ctors
  toSingClauses   <- mapM mkToSingClause ctors
  let singKindInst =
        InstanceD (map (singKindConstraint . VarT) tvbNames)
                  (AppT (ConT singKindClassName)
                        (kindParam k))
                  [ mkTyFamInst demoteRepName
                     [kindParam k]
                     (foldType (ConT name)
                       (map (AppT demote . kindParam . VarT) tvbNames))
                  , FunD fromSingName (fromSingClauses `orIfEmpty` emptyMethod aName)
                  , FunD toSingName   (toSingClauses   `orIfEmpty` emptyMethod aName) ]

  -- SEq instance
  sEqInsts <- if elem eqName derivings
              then mapM (mkEqualityInstance k ctors') [sEqClassDesc, sDecideClassDesc]
              else return []

  -- e.g. type SNat (a :: Nat) = Sing a
  let kindedSynInst =
        TySynD (singTyConName name)
               [KindedTV aName k]
               (AppT singFamily a)

  return $ (DataInstD [] singFamilyName [SigT a k] ctors' []) :
           kindedSynInst :
           singKindInst :
           sEqInsts ++
           ctorInstDecls
  where -- in the Rep case, the names of the constructors are in the wrong scope
        -- (they're types, not datacons), so we have to reinterpret them.
        mkConName :: Name -> Name
        mkConName = if rep then reinterpret else id

        mkFromSingClause :: Quasi q => Con -> q Clause
        mkFromSingClause c = do
          let (cname, numArgs) = extractNameArgs c
          varNames <- replicateM numArgs (qNewName "b")
          return $ Clause [ConP (singDataConName cname) (map VarP varNames)]
                          (NormalB $ foldExp
                             (ConE $ mkConName cname)
                             (map (AppE (VarE fromSingName) . VarE) varNames))
                          []

        mkToSingClause :: Quasi q => Con -> q Clause
        mkToSingClause = ctor1Case $ \cname types -> do
          varNames  <- mapM (const $ qNewName "b") types
          svarNames <- mapM (const $ qNewName "c") types
          promoted  <- mapM promoteType types
          let recursiveCalls = zipWith mkRecursiveCall varNames promoted
          return $
            Clause [ConP (mkConName cname) (map VarP varNames)]
                   (NormalB $
                    multiCase recursiveCalls
                              (map (ConP someSingDataName . listify . VarP)
                                   svarNames)
                              (AppE (ConE someSingDataName)
                                        (foldExp (ConE (singDataConName cname))
                                                 (map VarE svarNames))))
                   []

        mkRecursiveCall :: Name -> Kind -> Exp
        mkRecursiveCall var_name ki =
          SigE (AppE (VarE toSingName) (VarE var_name))
               (AppT (ConT someSingTypeName) (kindParam ki))

        emptyMethod :: Name -> [Clause]
        emptyMethod n = [Clause [VarP n] (NormalB $ CaseE (VarE n) emptyMatches) []]

singKind :: Quasi q => Kind -> q (Kind -> Kind)
singKind (ForallT _ _ _) =
  fail "Singling of explicitly quantified kinds not yet supported"
singKind (VarT _) = fail "Singling of kind variables not yet supported"
singKind (ConT _) = fail "Singling of named kinds not yet supported"
singKind (TupleT _) = fail "Singling of tuple kinds not yet supported"
singKind (UnboxedTupleT _) = fail "Unboxed tuple used as kind"
singKind ArrowT = fail "Singling of unsaturated arrow kinds not yet supported"
singKind ListT = fail "Singling of list kinds not yet supported"
singKind (AppT (AppT ArrowT k1) k2) = do
  k1fn <- singKind k1
  k2fn <- singKind k2
  k <- qNewName "k"
  return $ \f -> AppT (AppT ArrowT (k1fn (VarT k))) (k2fn (AppT f (VarT k)))
singKind (AppT _ _) = fail "Singling of kind applications not yet supported"
singKind (SigT _ _) =
  fail "Singling of explicitly annotated kinds not yet supported"
singKind (LitT _) = fail "Type literal used as kind"
singKind (PromotedT _) = fail "Promoted data constructor used as kind"
singKind (PromotedTupleT _) = fail "Promoted tuple used as kind"
singKind PromotedNilT = fail "Promoted nil used as kind"
singKind PromotedConsT = fail "Promoted cons used as kind"
singKind StarT = return $ \k -> AppT (AppT ArrowT k) StarT
singKind ConstraintT = fail "Singling of constraint kinds not yet supported"

singType :: Quasi q => Type -> q TypeFn
singType ty = do   -- replace with singTypeRec [] ty after GHC bug #??? is fixed
  sTypeFn <- singTypeRec [] ty
  return $ \inner_ty -> liftOutForalls $ sTypeFn inner_ty

-- Lifts all foralls to the top-level. This is a workaround for bug #8031 on GHC
-- Trac
liftOutForalls :: Type -> Type
liftOutForalls =
  go [] [] []
  where
    go tyvars cxt args (ForallT tyvars1 cxt1 t1)
      = go (reverse tyvars1 ++ tyvars) (reverse cxt1 ++ cxt) args t1
    go tyvars cxt args (SigT t1 _kind)  -- ignore these kind annotations, which have to be *
      = go tyvars cxt args t1
    go tyvars cxt args (AppT (AppT ArrowT arg1) res1)
      = go tyvars cxt (arg1 : args) res1
    go [] [] args t1
      = mk_fun_ty (reverse args) t1
    go tyvars cxt args t1
      = ForallT (reverse tyvars) (reverse cxt) (mk_fun_ty (reverse args) t1)

    mk_fun_ty [] res = res
    mk_fun_ty (arg1:args) res = AppT (AppT ArrowT arg1) (mk_fun_ty args res)

-- the first parameter is the list of types the current type is applied to
singTypeRec :: Quasi q => TypeContext -> Type -> q TypeFn
singTypeRec (_:_) (ForallT _ _ _) =
  fail "I thought this was impossible in Haskell. Email me at eir@cis.upenn.edu with your code if you see this message."
singTypeRec [] (ForallT _ [] ty) = -- Sing makes handling foralls automatic
  singTypeRec [] ty
singTypeRec ctx (ForallT _tvbs cxt innerty) = do
  cxt' <- singContext cxt
  innerty' <- singTypeRec ctx innerty
  return $ \ty -> ForallT [] cxt' (innerty' ty)
singTypeRec (_:_) (VarT _) =
  fail "Singling of type variables of arrow kinds not yet supported"
singTypeRec [] (VarT _name) =
  return $ \ty -> AppT singFamily ty
singTypeRec _ctx (ConT _name) = -- we don't need to process the context with Sing
  return $ \ty -> AppT singFamily ty
singTypeRec _ctx (TupleT _n) = -- just like ConT
  return $ \ty -> AppT singFamily ty
singTypeRec _ctx (UnboxedTupleT _n) =
  fail "Singling of unboxed tuple types not yet supported"
singTypeRec ctx ArrowT = case ctx of
  [ty1, ty2] -> do
    t <- qNewName "t"
    sty1 <- singTypeRec [] ty1
    sty2 <- singTypeRec [] ty2
    k1 <- promoteType ty1
    return (\f -> ForallT [KindedTV t k1]
                          []
                          (AppT (AppT ArrowT (sty1 (VarT t)))
                                (sty2 (AppT f (VarT t)))))
  _ -> fail "Internal error in Sing: converting ArrowT with improper context"
singTypeRec _ctx ListT =
  return $ \ty -> AppT singFamily ty
singTypeRec ctx (AppT ty1 ty2) =
  singTypeRec (ty2 : ctx) ty1 -- recur with the ty2 in the applied context
singTypeRec _ctx (SigT _ty _knd) =
  fail "Singling of types with explicit kinds not yet supported"
singTypeRec _ctx (LitT _) = fail "Singling of type-level literals not yet supported"
singTypeRec _ctx (PromotedT _) =
  fail "Singling of promoted data constructors not yet supported"
singTypeRec _ctx (PromotedTupleT _) =
  fail "Singling of type-level tuples not yet supported"
singTypeRec _ctx PromotedNilT = fail "Singling of promoted nil not yet supported"
singTypeRec _ctx PromotedConsT = fail "Singling of type-level cons not yet supported"
singTypeRec _ctx StarT = fail "* used as type"
singTypeRec _ctx ConstraintT = fail "Constraint used as type"

-- refine a constraint context
singContext :: Quasi q => Cxt -> q Cxt
singContext = mapM singPred

singPred :: Quasi q => Pred -> q Pred
singPred (ClassP name tys) = do
  kis <- mapM promoteType tys
  let sName = singClassName name
  return $ ClassP sName (map kindParam kis)
singPred (EqualP _ty1 _ty2) =
  fail "Singling of type equality constraints not yet supported"

singClause :: Quasi q => ExpTable -> Clause -> q Clause
singClause vars (Clause pats (NormalB exp) []) = do
  (sPats, vartbl) <- evalForPair $ mapM (singPat Parameter) pats
  let vars' = Map.union vartbl vars
  sBody <- NormalB <$> singExp vars' exp
  return $ Clause sPats sBody []
singClause _ (Clause _ (GuardedB _) _) =
  fail "Singling of guarded patterns not yet supported"
singClause _ (Clause _ _ (_:_)) =
  fail "Singling of <<where>> declarations not yet supported"

type ExpsQ q = QWithAux ExpTable q

-- we need to know where a pattern is to anticipate when
-- GHC's brain might explode
data PatternContext = LetBinding
                    | CaseStatement
                    | TopLevel
                    | Parameter
                    | Statement
                    deriving Eq

checkIfBrainWillExplode :: Quasi q => PatternContext -> ExpsQ q ()
checkIfBrainWillExplode CaseStatement = return ()
checkIfBrainWillExplode Statement = return ()
checkIfBrainWillExplode Parameter = return ()
checkIfBrainWillExplode _ =
  fail $ "Can't use a singleton pattern outside of a case-statement or\n" ++
         "do expression: GHC's brain will explode if you try. (Do try it!)"

-- convert a pattern, building up the lexical scope as we go
singPat :: Quasi q => PatternContext -> Pat -> ExpsQ q Pat
singPat _patCxt (LitP _lit) =
  fail "Singling of literal patterns not yet supported"
singPat patCxt (VarP name) =
  let new = if patCxt == TopLevel then singValName name else name in do
    addBinding name (VarE new)
    return $ VarP new
singPat patCxt (TupP pats) =
  singPat patCxt (ConP (tupleDataName (length pats)) pats)
singPat _patCxt (UnboxedTupP _pats) =
  fail "Singling of unboxed tuples not supported"
singPat patCxt (ConP name pats) = do
  checkIfBrainWillExplode patCxt
  pats' <- mapM (singPat patCxt) pats
  return $ ConP (singDataConName name) pats'
singPat patCxt (InfixP pat1 name pat2) = singPat patCxt (ConP name [pat1, pat2])
singPat _patCxt (UInfixP _ _ _) =
  fail "Singling of unresolved infix patterns not supported"
singPat _patCxt (ParensP _) =
  fail "Singling of unresolved paren patterns not supported"
singPat patCxt (TildeP pat) = do
  pat' <- singPat patCxt pat
  return $ TildeP pat'
singPat patCxt (BangP pat) = do
  pat' <- singPat patCxt pat
  return $ BangP pat'
singPat patCxt (AsP name pat) = do
  let new = if patCxt == TopLevel then singValName name else name in do
    pat' <- singPat patCxt pat
    addBinding name (VarE new)
    return $ AsP name pat'
singPat _patCxt WildP = return WildP
singPat _patCxt (RecP _name _fields) =
  fail "Singling of record patterns not yet supported"
singPat patCxt (ListP pats) = do
  checkIfBrainWillExplode patCxt
  sPats <- mapM (singPat patCxt) pats
  return $ foldr (\elt lst -> ConP sconsName [elt, lst]) (ConP snilName []) sPats
singPat _patCxt (SigP _pat _ty) =
  fail "Singling of annotated patterns not yet supported"
singPat _patCxt (ViewP _exp _pat) =
  fail "Singling of view patterns not yet supported"

singExp :: Quasi q => ExpTable -> Exp -> q Exp
singExp vars (VarE name) = case Map.lookup name vars of
  Just exp -> return exp
  Nothing -> return (singVal name)
singExp _vars (ConE name) = return $ singDataCon name
singExp _vars (LitE lit) = singLit lit
singExp vars (AppE exp1 exp2) = do
  exp1' <- singExp vars exp1
  exp2' <- singExp vars exp2
  return $ AppE exp1' exp2'
singExp vars (InfixE mexp1 exp mexp2) =
  case (mexp1, mexp2) of
    (Nothing, Nothing) -> singExp vars exp
    (Just exp1, Nothing) -> singExp vars (AppE exp exp1)
    (Nothing, Just _exp2) ->
      fail "Singling of right-only sections not yet supported"
    (Just exp1, Just exp2) -> singExp vars (AppE (AppE exp exp1) exp2)
singExp _vars (UInfixE _ _ _) =
  fail "Singling of unresolved infix expressions not supported"
singExp _vars (ParensE _) =
  fail "Singling of unresolved paren expressions not supported"
singExp vars (LamE pats exp) = do
  (pats', vartbl) <- evalForPair $ mapM (singPat Parameter) pats
  let vars' = Map.union vartbl vars -- order matters; union is left-biased
  exp' <- singExp vars' exp
  return $ LamE pats' exp'
singExp _vars (LamCaseE _matches) =
  fail "Singling of case expressions not yet supported"
singExp vars (TupE exps) = do
  sExps <- mapM (singExp vars) exps
  sTuple <- singExp vars (ConE (tupleDataName (length exps)))
  return $ foldExp sTuple sExps
singExp _vars (UnboxedTupE _exps) =
  fail "Singling of unboxed tuple not supported"
singExp vars (CondE bexp texp fexp) = do
  exps <- mapM (singExp vars) [bexp, texp, fexp]
  return $ foldExp (VarE sIfName) exps
singExp _vars (MultiIfE _alts) =
  fail "Singling of multi-way if statements not yet supported"
singExp _vars (LetE _decs _exp) =
  fail "Singling of let expressions not yet supported"
singExp _vars (CaseE _exp _matches) =
  fail "Singling of case expressions not yet supported"
singExp _vars (DoE _stmts) =
  fail "Singling of do expressions not yet supported"
singExp _vars (CompE _stmts) =
  fail "Singling of list comprehensions not yet supported"
singExp _vars (ArithSeqE _range) =
  fail "Singling of ranges not yet supported"
singExp vars (ListE exps) = do
  sExps <- mapM (singExp vars) exps
  return $ foldr (\x -> (AppE (AppE (ConE sconsName) x)))
                 (ConE snilName) sExps
singExp _vars (SigE _exp _ty) =
  fail "Singling of annotated expressions not yet supported"
singExp _vars (RecConE _name _fields) =
  fail "Singling of record construction not yet supported"
singExp _vars (RecUpdE _exp _fields) =
  fail "Singling of record updates not yet supported"

singLit :: Quasi q => Lit -> q Exp
singLit lit = SigE (VarE singMethName) <$> (AppT singFamily <$> (promoteLit lit))
