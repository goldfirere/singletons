{- Data/Singletons/Singletons.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to refine constructs to work with singleton
types. It is an internal module to the singletons package.
-}
{-# LANGUAGE PatternGuards, TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data.Singletons.Singletons where

import Language.Haskell.TH
import Data.Singletons.Exports
import Data.Singletons.Util
import Data.Singletons.Promote
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Writer
import Data.List

-- map to track bound variables
type ExpTable = Map.Map Name Exp

-- translating a type gives a type with a hole in it,
-- represented here as a function
type TypeFn = Type -> Type

-- a list of argument types extracted from a type application
type TypeContext = [Type]

singFamilyName, isSingletonName, forgettableName, comboClassName, witnessName,
  demoteName, singKindClassName, singInstanceMethName, singInstanceTyConName,
  singInstanceDataConName, sEqClassName, sEqMethName, sconsName, snilName,
  smartSconsName, smartSnilName, sIfName, undefinedName, kindParamName,
  ofKindName :: Name
singFamilyName = ''Sing
isSingletonName = ''SingI
forgettableName = ''SingE
comboClassName = ''SingRep
witnessName = 'sing
forgetName = 'fromSing
demoteName = ''DemoteRep
singKindClassName = ''SingKind
singInstanceMethName = 'singInstance
singInstanceTyConName = ''SingInstance
singInstanceDataConName = 'SingInstance
sEqClassName = mkName "SEq"
sEqMethName = mkName "%==%"
sconsName = mkName "SCons"
snilName = mkName "SNil"
smartSconsName = mkName "sCons"
smartSnilName = mkName "sNil"
sIfName = mkName "sIf"
undefinedName = 'undefined
kindParamName = 'KindParam
ofKindName = ''KindIs

mkTupleName :: Int -> Name
mkTupleName n = mkName $ "STuple" ++ (show n)

singFamily :: Type
singFamily = ConT singFamilyName

singKindConstraint :: Kind -> Pred
singKindConstraint k = ClassP singKindClassName [kindParam k]

singInstanceMeth :: Exp
singInstanceMeth = VarE singInstanceMethName

singInstanceTyCon :: Type
singInstanceTyCon = ConT singInstanceTyConName

singInstanceDataCon :: Exp
singInstanceDataCon = ConE singInstanceDataConName

singInstancePat :: Pat
singInstancePat = ConP singInstanceDataConName []

demote :: Type
demote = ConT demoteName

singDataConName :: Name -> Name
singDataConName nm = case nameBase nm of
  "[]" -> snilName
  ":"  -> sconsName
  tuple | isTupleString tuple -> mkTupleName (tupleDegree tuple)
  _ -> prefixUCName "S" ":%" nm

singTyConName :: Name -> Name
singTyConName name | nameBase name == "[]" = mkName "SList"
                   | isTupleName name = mkTupleName (tupleDegree $ nameBase name)
                   | otherwise        = prefixUCName "S" ":%" name

singClassName :: Name -> Name
singClassName = singTyConName

singDataCon :: Name -> Exp
singDataCon = ConE . singDataConName

smartConName :: Name -> Name
smartConName = locase . singDataConName

smartCon :: Name -> Exp
smartCon = VarE . smartConName

singValName :: Name -> Name
singValName n
  | nameBase n == "undefined" = undefinedName
  | otherwise                 = (prefixLCName "s" "%") $ upcase n

singVal :: Name -> Exp
singVal = VarE . singValName

kindParam :: Kind -> Type
kindParam k = SigT (ConT kindParamName) (AppT (ConT ofKindName) k)

-- generate singleton definitions from an ADT
genSingletons :: [Name] -> Q [Dec]
genSingletons names = do
  checkForRep names
  infos <- mapM reifyWithWarning names
  decls <- mapM singInfo infos
  return $ concat decls

singInfo :: Info -> Q [Dec]
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
singCtor :: Type -> Con -> QWithDecs Con 
singCtor a = ctorCases
  (\name types -> do
    let sName = singDataConName name
        sCon = singDataCon name
        pCon = promoteDataCon name
    indexNames <- lift $ replicateM (length types) (newName "n")
    let indices = map VarT indexNames
    kinds <- lift $ mapM promoteType types
    args <- lift $ buildArgTypes types indices
    let tvbs = zipWith KindedTV indexNames kinds
        bareKindVars = filter isVarK kinds

    -- SingI instance
    addElement $ InstanceD ((map singKindConstraint bareKindVars) ++
                            (map (ClassP comboClassName . return) indices))
                           (AppT (ConT isSingletonName)
                                 (foldType pCon (zipWith SigT indices kinds)))
                           [ValD (VarP witnessName)
                                 (NormalB $ foldExp sCon (replicate (length types)
                                                           (VarE witnessName)))
                                 []]

    -- smart constructor type signature
    smartConType <- lift $ conTypesToFunType indexNames args kinds
                                      (AppT singFamily (foldType pCon indices))
    addElement $ SigD (smartConName name) (liftOutForalls smartConType)
     
    -- smart constructor
    let vars = map VarE indexNames
        smartConBody = mkSingInstances vars (foldExp (singDataCon name) vars)
    addElement $ FunD (smartConName name)
                      [Clause (map VarP indexNames)
                        (NormalB smartConBody)
                        []]

    return $ ForallC tvbs
                     ((EqualP a (foldType (promoteDataCon name) indices)) :
                       (map (ClassP comboClassName . return) indices) ++
                       (map singKindConstraint bareKindVars))
                     (NormalC sName $ map (\ty -> (NotStrict,ty)) args))
  (\_tvbs cxt ctor -> case cxt of
    _:_ -> fail "Singling of constrained constructors not yet supported"
    [] -> singCtor a ctor)
  where buildArgTypes :: [Type] -> [Type] -> Q [Type]
        buildArgTypes types indices = do
          typeFns <- mapM (singType False) types
          return $ zipWith id typeFns indices

        conTypesToFunType :: [Name] -> [Type] -> [Kind] -> Type -> Q Type
        conTypesToFunType [] [] [] ret = return ret
        conTypesToFunType (nm : nmtail) (ty : tytail) (k : ktail) ret = do
          rhs <- conTypesToFunType nmtail tytail ktail ret    
          let innerty = AppT (AppT ArrowT ty) rhs
          return $ ForallT [KindedTV nm k]
                           (if isVarK k then [singKindConstraint k] else [])
                           innerty
        conTypesToFunType _ _ _ _ =
          fail "Internal error in conTypesToFunType"

        mkSingInstances :: [Exp] -> Exp -> Exp
        mkSingInstances [] exp = exp
        mkSingInstances (var:tail) exp =
          CaseE (AppE singInstanceMeth var)
                [Match singInstancePat (NormalB $ mkSingInstances tail exp) []]

-- refine the declarations given
singletons :: Q [Dec] -> Q [Dec]
singletons qdec = do
  decls <- qdec
  singDecs decls

singDecs :: [Dec] -> Q [Dec]
singDecs decls = do
  (promDecls, badNames) <- promoteDecs decls
  -- need to remove the bad names returned from promoteDecs
  newDecls <- mapM singDec
                   (filter (\dec ->
                     not $ or (map (\f -> f dec)
                              (map containsName badNames))) decls)
  return $ decls ++ promDecls ++ (concat newDecls)

singDec :: Dec -> Q [Dec]
singDec (FunD name clauses) = do
  let sName = singValName name
      vars = Map.singleton name (VarE sName)
  liftM return $ funD sName (map (singClause vars) clauses)
singDec (ValD _ (GuardedB _) _) =
  fail "Singling of definitions of values with a pattern guard not yet supported"
singDec (ValD _ _ (_:_)) =
  fail "Singling of definitions of values with a <<where>> clause not yet supported"
singDec (ValD pat (NormalB exp) []) = do
  (sPat, vartbl) <- evalForPair $ singPat TopLevel pat
  sExp <- singExp vartbl exp
  return [ValD sPat (NormalB sExp) []]
singDec (DataD (_:_) _ _ _ _) =
  fail "Singling of constrained datatypes not supported"
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
  tyTrans <- singType True ty
  return [SigD (singValName name) (tyTrans (promoteVal name))]
singDec (ForeignD fgn) =
  let name = extractName fgn in do
    reportWarning $ "Singling of foreign functions not supported -- " ++
                    (show name) ++ " ignored"
    return []
  where extractName :: Foreign -> Name
        extractName (ImportF _ _ _ n _) = n
        extractName (ExportF _ _ n _) = n
singDec (InfixD fixity name)
  | isUpcase name = return [InfixD fixity (singDataConName name)]
  | otherwise     = return [InfixD fixity (singValName name)]
singDec (PragmaD _prag) = do
    reportWarning "Singling of pragmas not supported"
    return []
singDec (FamilyD _flavour _name _tvbs _mkind) =
  fail "Singling of type and data families not yet supported"
singDec (DataInstD _cxt _name _tys _ctors _derivings) = 
  fail "Singling of data instances not yet supported"
singDec (NewtypeInstD _cxt _name _tys _ctor _derivings) =
  fail "Singling of newtype instances not yet supported"
#if __GLASGOW_HASKELL__ >= 707
singDec (ClosedTypeFamilyD _name _tvs _mkind _eqns) =
  fail "Singling of closed type families not yet supported"
singDec (TySynInstD _name _eqns) =
#else
singDec (TySynInstD _name _lhs _rhs) =
#endif
  fail "Singling of type family instances not yet supported"

-- create instances of SEq for each type in the list
singEqInstances :: [Name] -> Q [Dec]
singEqInstances = concatMapM singEqInstance

-- create instance of SEq for the given *singleton* type
singEqInstance :: Name -> Q [Dec]
singEqInstance name = do
  promotion <- promoteEqInstance name
  (tvbs, cons) <- getDataD "I cannot make an instance of SEq for it." name
  let tyvars = map (VarT . extractTvbName) tvbs
      kind = foldType (ConT name) tyvars
  aName <- newName "a"
  let aVar = VarT aName
  scons <- mapM (evalWithoutAux . singCtor aVar) cons
  dec <- mkSingEqInstance kind scons
  return $ dec : promotion

-- create an SEq instance for singletons of the given kind,
-- with the given *singleton* constructors 
mkSingEqInstance :: Kind -> [Con] -> Q Dec
mkSingEqInstance k ctors = do
  let ctorPairs = [ (c1, c2) | c1 <- ctors, c2 <- ctors ]
  sEqMethClauses <- mapM mkEqMethClause ctorPairs
  return $ InstanceD (map (\k -> ClassP sEqClassName [kindParam k])
                          (getBareKinds ctors))
                     (AppT (ConT sEqClassName)
                           (kindParam k))
                     [FunD sEqMethName sEqMethClauses]
  where mkEqMethClause :: (Con, Con) -> Q Clause
        mkEqMethClause (c1, c2) =
          if c1 == c2
          then do
            let (name, numArgs) = extractNameArgs c1
            lnames <- replicateM numArgs (newName "a")
            rnames <- replicateM numArgs (newName "b")
            let lpats = map VarP lnames
                rpats = map VarP rnames
                lvars = map VarE lnames
                rvars = map VarE rnames
            return $ Clause
              [ConP name lpats, ConP name rpats]
              (NormalB $
                allExp (zipWith (\l r -> foldExp (VarE sEqMethName) [l, r])
                                lvars rvars))
              []
          else do
            let (lname, lNumArgs) = extractNameArgs c1
                (rname, rNumArgs) = extractNameArgs c2
            return $ Clause
              [ConP lname (replicate lNumArgs WildP),
               ConP rname (replicate rNumArgs WildP)]
              (NormalB (singDataCon falseName))
              []

        getBareKinds :: [Con] -> [Kind]
        getBareKinds = foldl (\res -> ctorCases
          (\_ _ -> res) -- must be a constant constructor
          (\tvbs _ _ -> union res (filter isVarK $ map extractTvbKind tvbs)))
          []

        allExp :: [Exp] -> Exp
        allExp [] = singDataCon trueName
        allExp [one] = one
        allExp (h:t) = AppE (AppE (singVal andName) h) (allExp t)

-- the first parameter is True when we're refining the special case "Rep"
-- and false otherwise. We wish to consider the promotion of "Rep" to be *
-- not a promoted data constructor.
singDataD :: Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> [Name] -> Q [Dec]
singDataD rep cxt name tvbs ctors derivings
  | (_:_) <- cxt = fail "Singling of constrained datatypes is not supported"
  | otherwise    = do
  aName <- newName "a"
  let a = VarT aName
  let tvbNames = map extractTvbName tvbs
  k <- promoteType (foldType (ConT name) (map VarT tvbNames))
  (ctors', ctorInstDecls) <- evalForPair $ mapM (singCtor a) ctors
  
  -- instance for SingKind
  let singKindInst =
        InstanceD []
                  (AppT (ConT singKindClassName)
                        (kindParam k))
                  [FunD singInstanceMethName
                        (map mkSingInstanceClause ctors')]
  
  -- SEq instance
  sEqInst <- mkSingEqInstance k ctors'
  
  -- e.g. type SNat (a :: Nat) = Sing a
  let kindedSynInst =
        TySynD (singTyConName name)
               [KindedTV aName k]
               (AppT singFamily a)

  -- SingE instance
  forgetClauses <- mapM mkForgetClause ctors
  let singEInst =
        InstanceD []
                  (AppT (ConT forgettableName) (kindParam k))
                  [mkTyFamInst demoteName
                    [kindParam k]
                    (foldType (ConT name)
                      (map (\kv -> AppT demote (kindParam (VarT kv)))
                           tvbNames)),
                   FunD forgetName
                        forgetClauses]

  return $ (if (any (\n -> (nameBase n) == "Eq") derivings)
            then (sEqInst :)
            else id) $
             (DataInstD [] singFamilyName [SigT a k] ctors' []) :
             singEInst :
             kindedSynInst :
             singKindInst :
             ctorInstDecls
  where mkSingInstanceClause :: Con -> Clause
        mkSingInstanceClause = ctor1Case
          (\nm tys ->
            Clause [ConP nm (replicate (length tys) WildP)]
                   (NormalB singInstanceDataCon) [])

        mkForgetClause :: Con -> Q Clause
        mkForgetClause c = do
          let (name, numArgs) = extractNameArgs c
          varNames <- replicateM numArgs (newName "a")
          return $ Clause [ConP (singDataConName name) (map VarP varNames)]
                          (NormalB $ foldExp
                             (ConE $ (if rep then reinterpret else id) name)
                             (map (AppE (VarE forgetName) . VarE) varNames))
                          []

singKind :: Kind -> Q (Kind -> Kind)
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
  k <- newName "k"
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

-- the first parameter is whether or not this type occurs in a positive position
singType :: Bool -> Type -> Q TypeFn
singType pos ty = do   -- replace with singTypeRec [] pos ty after GHC bug #??? is fixed
  sTypeFn <- singTypeRec [] pos ty
  return $ \inner_ty -> liftOutForalls $ sTypeFn inner_ty

  -- the lifts all foralls to the top-level
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
-- the second parameter is whether or not this type occurs in a positive position
singTypeRec :: TypeContext -> Bool -> Type -> Q TypeFn
singTypeRec (_:_) _pos (ForallT _ _ _) =
  fail "I thought this was impossible in Haskell. Email me at eir@cis.upenn.edu with your code if you see this message."
singTypeRec [] pos (ForallT _ [] ty) = -- Sing makes handling foralls automatic
  singTypeRec [] pos ty
singTypeRec ctx pos (ForallT _tvbs cxt innerty) = do
  cxt' <- singContext cxt
  innerty' <- singTypeRec ctx pos innerty
  return $ \ty -> ForallT [] cxt' (innerty' ty)
singTypeRec (_:_) _pos (VarT _) =
  fail "Singling of type variables of arrow kinds not yet supported"
singTypeRec [] _pos (VarT _name) = 
  return $ \ty -> AppT singFamily ty
singTypeRec _ctx _pos (ConT _name) = -- we don't need to process the context with Sing
  return $ \ty -> AppT singFamily ty
singTypeRec _ctx _pos (TupleT _n) = -- just like ConT
  return $ \ty -> AppT singFamily ty
singTypeRec _ctx _pos (UnboxedTupleT _n) =
  fail "Singling of unboxed tuple types not yet supported"
singTypeRec ctx pos ArrowT = case ctx of
  [ty1, ty2] -> do
    t <- newName "t"
    sty1 <- singTypeRec [] (not pos) ty1
    sty2 <- singTypeRec [] pos ty2
    k1 <- promoteType ty1
    -- need a SingKind constraint on all kind variables that appear
    -- outside of any kind constructor in a negative position (to the
    -- left of an odd number of arrows)
    let polykinds = extractPolyKinds (not pos) k1
    return (\f -> ForallT [KindedTV t k1]
                          (map (\k -> ClassP singKindClassName [kindParam k]) polykinds)
                          (AppT (AppT ArrowT (sty1 (VarT t)))
                                (sty2 (AppT f (VarT t)))))
    where extractPolyKinds :: Bool -> Kind -> [Kind]
          extractPolyKinds pos (AppT (AppT ArrowT k1) k2) =
            (extractPolyKinds (not pos) k1) ++ (extractPolyKinds pos k2)
          extractPolyKinds False (VarT k) = [VarT k]
          extractPolyKinds _ _ = []
  _ -> fail "Internal error in Sing: converting ArrowT with improper context"
singTypeRec _ctx _pos ListT =
  return $ \ty -> AppT singFamily ty
singTypeRec ctx pos (AppT ty1 ty2) =
  singTypeRec (ty2 : ctx) pos ty1 -- recur with the ty2 in the applied context
singTypeRec _ctx _pos (SigT _ty _knd) =
  fail "Singling of types with explicit kinds not yet supported"
singTypeRec _ctx _pos (LitT _) = fail "Singling of type-level literals not yet supported"
singTypeRec _ctx _pos (PromotedT _) =
  fail "Singling of promoted data constructors not yet supported"
singTypeRec _ctx _pos (PromotedTupleT _) =
  fail "Singling of type-level tuples not yet supported"
singTypeRec _ctx _pos PromotedNilT = fail "Singling of promoted nil not yet supported"
singTypeRec _ctx _pos PromotedConsT = fail "Singling of type-level cons not yet supported"
singTypeRec _ctx _pos StarT = fail "* used as type"
singTypeRec _ctx _pos ConstraintT = fail "Constraint used as type"

-- refine a constraint context
singContext :: Cxt -> Q Cxt
singContext = mapM singPred

singPred :: Pred -> Q Pred
singPred (ClassP name tys) = do
  kis <- mapM promoteType tys
  let sName = singClassName name
  return $ ClassP sName (map kindParam kis)
singPred (EqualP _ty1 _ty2) =
  fail "Singling of type equality constraints not yet supported"

singClause :: ExpTable -> Clause -> Q Clause
singClause vars (Clause pats (NormalB exp) []) = do
  (sPats, vartbl) <- evalForPair $ mapM (singPat Parameter) pats
  let vars' = Map.union vartbl vars
  sBody <- normalB $ singExp vars' exp
  return $ Clause sPats sBody []
singClause _ (Clause _ (GuardedB _) _) =
  fail "Singling of guarded patterns not yet supported"
singClause _ (Clause _ _ (_:_)) =
  fail "Singling of <<where>> declarations not yet supported"

type ExpsQ = QWithAux ExpTable

-- we need to know where a pattern is to anticipate when
-- GHC's brain might explode
data PatternContext = LetBinding
                    | CaseStatement
                    | TopLevel
                    | Parameter
                    | Statement
                    deriving Eq

checkIfBrainWillExplode :: PatternContext -> ExpsQ ()
checkIfBrainWillExplode CaseStatement = return ()
checkIfBrainWillExplode Statement = return ()
checkIfBrainWillExplode Parameter = return ()
checkIfBrainWillExplode _ =
  fail $ "Can't use a singleton pattern outside of a case-statement or\n" ++
         "do expression: GHC's brain will explode if you try. (Do try it!)"

-- convert a pattern, building up the lexical scope as we go
singPat :: PatternContext -> Pat -> ExpsQ Pat
singPat _patCxt (LitP _lit) =
  fail "Singling of literal patterns not yet supported"
singPat patCxt (VarP name) =
  let newName = if patCxt == TopLevel then singValName name else name in do
    addBinding name (VarE newName)
    return $ VarP newName
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
  let newName = if patCxt == TopLevel then singValName name else name in do
    pat' <- singPat patCxt pat
    addBinding name (VarE newName)
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

singExp :: ExpTable -> Exp -> Q Exp
singExp vars (VarE name) = case Map.lookup name vars of
  Just exp -> return exp
  Nothing -> return (singVal name)
singExp _vars (ConE name) = return $ smartCon name
singExp _vars (LitE _lit) =
  fail "Singling of literal expressions not yet supported"
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
  return $ foldr (\x -> (AppE (AppE (VarE smartSconsName) x)))
                 (VarE smartSnilName) sExps
singExp _vars (SigE _exp _ty) =
  fail "Singling of annotated expressions not yet supported"
singExp _vars (RecConE _name _fields) =
  fail "Singling of record construction not yet supported"
singExp _vars (RecUpdE _exp _fields) =
  fail "Singling of record updates not yet supported"
