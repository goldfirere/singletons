{- Data/Singletons/Promote.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to promote term-level constructs to the
type level. It is an internal module to the singletons package.
-}

{-# LANGUAGE TemplateHaskell, CPP #-}

module Data.Singletons.Promote where

import Language.Haskell.TH hiding ( Q, cxt )
import Language.Haskell.TH.Syntax ( falseName, trueName, Quasi(..) )
import Data.Singletons.Util
import Data.Singletons.Types
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)
import Prelude hiding (exp)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List

anyTypeName, boolName, andName, tyEqName, repName, ifName,
  headName, tailName, symbolName :: Name
anyTypeName = ''Any
boolName = ''Bool
andName = '(&&)
#if __GLASGOW_HASKELL__ >= 707
tyEqName = ''(==)
#else
tyEqName = ''(:==)
#endif
repName = mkName "Rep"
ifName = ''If
headName = mkName "Head"  -- these will go away with the th-desugar change
tailName = mkName "Tail"
symbolName = ''Symbol

falseTy :: Type
falseTy = PromotedT falseName

trueTy :: Type
trueTy = PromotedT trueName

boolTy :: Type
boolTy = ConT boolName

andTy :: Type
andTy = promoteVal andName

ifTyFam :: Type
ifTyFam = ConT ifName

headTyFam :: Type
headTyFam = ConT headName

tailTyFam :: Type
tailTyFam = ConT tailName

promoteInfo :: Quasi q => Info -> q [Dec]
promoteInfo (ClassI _dec _instances) =
  fail "Promotion of class info not supported"
promoteInfo (ClassOpI _name _ty _className _fixity) =
  fail "Promotion of class members info not supported"
promoteInfo (TyConI dec) = evalWithoutAux $ promoteDec Map.empty dec
promoteInfo (FamilyI _dec _instances) =
  fail "Promotion of type family info not yet supported" -- KindFams
promoteInfo (PrimTyConI _name _numArgs _unlifted) =
  fail "Promotion of primitive type constructors not supported"
promoteInfo (DataConI _name _ty _tyname _fixity) =
  fail $ "Promotion of individual constructors not supported; " ++
         "promote the type instead"
promoteInfo (VarI _name _ty _mdec _fixity) =
  fail "Promotion of value info not supported"
promoteInfo (TyVarI _name _ty) =
  fail "Promotion of type variable info not supported"

promoteValName :: Name -> Name
promoteValName n
  | nameBase n == "undefined" = anyTypeName
  | otherwise                 = upcase n

promoteVal :: Name -> Type
promoteVal = ConT . promoteValName

promoteType :: Quasi q => Type -> q Kind
-- We don't need to worry about constraints: they are used to express
-- static guarantees at runtime. But, because we don't need to do
-- anything special to keep static guarantees at compile time, we don't
-- need to promote them.
promoteType (ForallT _tvbs _ ty) = promoteType ty -- ForallKinds
promoteType (VarT name) = return $ VarT name
promoteType (ConT name) = return $
  case nameBase name of
    "TypeRep"                 -> StarT
    "String"                  -> ConT symbolName
    x | x == nameBase repName -> StarT
      | otherwise             -> ConT name
promoteType (TupleT n) = return $ TupleT n
promoteType (UnboxedTupleT _n) = fail "Promotion of unboxed tuples not supported"
promoteType ArrowT = return ArrowT
promoteType ListT = return ListT
promoteType (AppT (AppT ArrowT (ForallT (_:_) _ _)) _) =
  fail "Cannot promote types of rank above 1."
promoteType (AppT ty1 ty2) = do
  k1 <- promoteType ty1
  k2 <- promoteType ty2
  return $ AppT k1 k2
promoteType (SigT _ty _) = fail "Cannot promote type of kind other than *"
promoteType (LitT _) = fail "Cannot promote a type-level literal"
promoteType (PromotedT _) = fail "Cannot promote a promoted data constructor"
promoteType (PromotedTupleT _) = fail "Cannot promote tuples that are already promoted"
promoteType PromotedNilT = fail "Cannot promote a nil that is already promoted"
promoteType PromotedConsT = fail "Cannot promote a cons that is already promoted"
promoteType StarT = fail "* used as a type"
promoteType ConstraintT = fail "Constraint used as a type"

-- a table to keep track of variable->type mappings
type TypeTable = Map.Map Name Type

-- | Promote every declaration given to the type level, retaining the originals.
promote :: Quasi q => q [Dec] -> q [Dec]
promote qdec = do
  decls <- qdec
  promDecls <- promoteDecs decls
  return $ decls ++ promDecls

-- | Promote each declaration, discarding the originals.
promoteOnly :: Quasi q => q [Dec] -> q [Dec]
promoteOnly qdec = do
  decls <- qdec
  promDecls <- promoteDecs decls
  return promDecls

checkForRep :: Quasi q => [Name] -> q ()
checkForRep names =
  when (any ((== nameBase repName) . nameBase) names)
    (fail $ "A data type named <<Rep>> is a special case.\n" ++
            "Promoting it will not work as expected.\n" ++
            "Please choose another name for your data type.")

checkForRepInDecls :: Quasi q => [Dec] -> q ()
checkForRepInDecls decls =
  checkForRep (map extractNameFromDec decls)
  where extractNameFromDec :: Dec -> Name
        extractNameFromDec (DataD _ name _ _ _) = name
        extractNameFromDec (NewtypeD _ name _ _ _) = name
        extractNameFromDec (TySynD name _ _) = name
        extractNameFromDec (FamilyD _ name _ _) = name
        extractNameFromDec _ = mkName "NotRep"

-- Note [Promoting declarations in two stages]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Promoting declarations proceeds in two stages:
-- 1) Promote everything except type signatures
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

-- Promote a list of declarations.
promoteDecs :: Quasi q => [Dec] -> q [Dec]
promoteDecs decls = do
  checkForRepInDecls decls
  let vartbl = Map.empty
  -- See Note [Promoting declarations in two stages]
  (newDecls, table) <- evalForPair $ mapM (promoteDec vartbl) decls
  (declss, namess) <- mapAndUnzipM (promoteDec' table) decls
  let moreNewDecls = concat declss
      names = concat namess
      noTypeSigs = Set.toList $ Set.difference (Map.keysSet $
#if __GLASGOW_HASKELL__ >= 707
                                                  Map.filter ((>= 0) . fst) table)
#else
                                                  Map.filter (>= 0) table)
#endif
                                               (Set.fromList names)
  when (not . null $ noTypeSigs) $ fail ("No type signature for functions: "
    ++ intercalate ", " (map (show . nameBase) noTypeSigs)
    ++ "; cannot promote or make singletons.")         
  return (concat newDecls ++ moreNewDecls)

-- | Produce instances for '(:==)' (type-level equality) from the given types
promoteEqInstances :: Quasi q => [Name] -> q [Dec]
promoteEqInstances = concatMapM promoteEqInstance

-- | Produce an instance for '(:==)' (type-level equality) from the given type
promoteEqInstance :: Quasi q => Name -> q [Dec]
promoteEqInstance name = do
  (_tvbs, cons) <- getDataD "I cannot make an instance of (:==:) for it." name
#if __GLASGOW_HASKELL__ >= 707
  vars <- replicateM (length _tvbs) (qNewName "k")
  let tyvars = map VarT vars
      kind = foldType (ConT name) tyvars
  inst_decs <- mkEqTypeInstance kind cons
  return inst_decs
#else
  let pairs = [(c1, c2) | c1 <- cons, c2 <- cons]
  mapM mkEqTypeInstance pairs
#endif

#if __GLASGOW_HASKELL__ >= 707

-- produce a closed type family helper and the instance
-- for (:==) over the given list of ctors
mkEqTypeInstance :: Quasi q => Kind -> [Con] -> q [Dec]
mkEqTypeInstance kind cons = do
  helperName <- newUniqueName "Equals"
  aName <- qNewName "a"
  bName <- qNewName "b"
  true_branches <- mapM mk_branch cons
  false_branch  <- false_case
  let closedFam = ClosedTypeFamilyD helperName
                                    [ KindedTV aName kind
                                    , KindedTV bName kind ]
                                    (Just boolTy)
                                    (true_branches ++ [false_branch])
      eqInst = TySynInstD tyEqName (TySynEqn [ SigT (VarT aName) kind
                                             , SigT (VarT bName) kind ]
                                             (foldType (ConT helperName)
                                                       [VarT aName, VarT bName]))
  return [closedFam, eqInst]

  where mk_branch :: Quasi q => Con -> q TySynEqn
        mk_branch con = do
          let (name, numArgs) = extractNameArgs con
          lnames <- replicateM numArgs (qNewName "a")
          rnames <- replicateM numArgs (qNewName "b")
          let lvars = map VarT lnames
              rvars = map VarT rnames
              ltype = foldType (PromotedT name) lvars
              rtype = foldType (PromotedT name) rvars
              results = zipWith (\l r -> foldType (ConT tyEqName) [l, r]) lvars rvars
              result = tyAll results
          return $ TySynEqn [ltype, rtype] result

        false_case :: Quasi q => q TySynEqn
        false_case = do
          lvar <- qNewName "a"
          rvar <- qNewName "b"
          return $ TySynEqn [SigT (VarT lvar) kind, SigT (VarT rvar) kind] falseTy

        tyAll :: [Type] -> Type -- "all" at the type level
        tyAll [] = trueTy
        tyAll [one] = one
        tyAll (h:t) = foldType andTy [h, (tyAll t)]

#else

-- produce the type instance for (:==) for the given pair of constructors
mkEqTypeInstance :: Quasi q => (Con, Con) -> q Dec
mkEqTypeInstance (c1, c2) =
  if c1 == c2
  then do
    let (name, numArgs) = extractNameArgs c1
    lnames <- replicateM numArgs (qNewName "a")
    rnames <- replicateM numArgs (qNewName "b")
    let lvars = map VarT lnames
        rvars = map VarT rnames
    return $ TySynInstD
      tyEqName
      [foldType (PromotedT name) lvars,
       foldType (PromotedT name) rvars]
      (tyAll (zipWith (\l r -> foldType (ConT tyEqName) [l, r])
                      lvars rvars))
  else do
    let (lname, lNumArgs) = extractNameArgs c1
        (rname, rNumArgs) = extractNameArgs c2
    lnames <- replicateM lNumArgs (qNewName "a")
    rnames <- replicateM rNumArgs (qNewName "b")
    return $ TySynInstD
      tyEqName
      [foldType (PromotedT lname) (map VarT lnames),
       foldType (PromotedT rname) (map VarT rnames)]
      falseTy
  where tyAll :: [Type] -> Type -- "all" at the type level
        tyAll [] = trueTy
        tyAll [one] = one
        tyAll (h:t) = foldType andTy [h, (tyAll t)]

#endif

-- keeps track of the number of non-uniform parameters to promoted values
-- and all of the instance equations for those values
#if __GLASGOW_HASKELL__ >= 707
type PromoteTable = Map.Map Name (Int, [TySynEqn])
#else
type PromoteTable = Map.Map Name Int
#endif
type PromoteQ q = QWithAux PromoteTable q

-- used when a type is declared as a type synonym, not a type family
-- no need to declare "type family ..." for these
typeSynonymFlag :: Int
typeSynonymFlag = -1

promoteDec :: Quasi q => TypeTable -> Dec -> PromoteQ q [Dec]
promoteDec vars (FunD name clauses) = do
  let proName = promoteValName name
      vars' = Map.insert name (promoteVal name) vars
      numArgs = getNumPats (head clauses) -- count the parameters
      -- Haskell requires all clauses to have the same number of parameters
  (eqns, instDecls) <- evalForPair $
                       mapM (promoteClause vars' proName) clauses
#if __GLASGOW_HASKELL__ >= 707
  addBinding name (numArgs, eqns) -- remember the number of parameters and the eqns
  return instDecls
#else
  addBinding name numArgs -- remember the number of parameters
  return $ eqns ++ instDecls
#endif
  where getNumPats :: Clause -> Int
        getNumPats (Clause pats _ _) = length pats
promoteDec vars (ValD pat body decs) = do
  -- see also the comment for promoteTopLevelPat
  when (length decs > 0)
    (fail $ "Promotion of global variable with <<where>> clause " ++
                "not yet supported")
  (rhs, decls) <- evalForPair $ promoteBody vars body
  (lhss, decls') <- evalForPair $ promoteTopLevelPat pat
  -- just use "type" decls
#if __GLASGOW_HASKELL__ >= 707
  mapM_ (flip addBinding (typeSynonymFlag, [])) (map lhsRawName lhss)
#else
  mapM_ (flip addBinding typeSynonymFlag) (map lhsRawName lhss)
#endif
  return $ (map (\(LHS _ nm hole) -> TySynD nm [] (hole rhs)) lhss) ++
           decls ++ decls'
promoteDec vars (DataD cxt name tvbs ctors derivings) =
  promoteDataD vars cxt name tvbs ctors derivings
promoteDec vars (NewtypeD cxt name tvbs ctor derivings) =
  promoteDataD vars cxt name tvbs [ctor] derivings
promoteDec _vars (TySynD _name _tvbs _ty) =
  fail "Promotion of type synonym declaration not yet supported"
promoteDec _vars (ClassD _cxt _name _tvbs _fundeps _decs) =
  fail "Promotion of class declaration not yet supported"
promoteDec _vars (InstanceD _cxt _ty _decs) =
  fail "Promotion of instance declaration not yet supported"
promoteDec _vars (SigD _name _ty) = return [] -- handle in promoteDec'
promoteDec _vars (ForeignD _fgn) =
  fail "Promotion of foreign function declaration not yet supported"
promoteDec _vars (InfixD fixity name)
  | isUpcase name = return [] -- automatic: promoting a type or data ctor
  | otherwise     = return [InfixD fixity (promoteValName name)] -- value
promoteDec _vars (PragmaD _prag) =
  fail "Promotion of pragmas not yet supported"
promoteDec _vars (FamilyD _flavour _name _tvbs _mkind) =
  fail "Promotion of type and data families not yet supported"
promoteDec _vars (DataInstD _cxt _name _tys _ctors _derivings) =
  fail "Promotion of data instances not yet supported"
promoteDec _vars (NewtypeInstD _cxt _name _tys _ctors _derivings) =
  fail "Promotion of newtype instances not yet supported"
#if __GLASGOW_HASKELL__ >= 707
promoteDec _vars (RoleAnnotD _name _roles) =
  return [] -- silently ignore role annotations, as they're harmless here
promoteDec _vars (ClosedTypeFamilyD _name _tvs _mkind _eqns) =
  fail "Promotion of closed type families not yet supported"
promoteDec _vars (TySynInstD _name _eqn) =
#else
promoteDec _vars (TySynInstD _name _lhs _rhs) =
#endif
  fail "Promotion of type synonym instances not yet supported"

-- only need to check if the datatype derives Eq. The rest is automatic.
promoteDataD :: Quasi q => TypeTable -> Cxt -> Name -> [TyVarBndr] -> [Con] ->
                [Name] -> PromoteQ q [Dec]
promoteDataD _vars _cxt _name _tvbs ctors derivings =
  if any (\n -> (nameBase n) == "Eq") derivings
    then do
#if __GLASGOW_HASKELL__ >= 707
      kvs <- replicateM (length _tvbs) (qNewName "k")
      inst_decs <- mkEqTypeInstance (foldType (ConT _name) (map VarT kvs)) ctors
      return inst_decs
#else
      let pairs = [ (c1, c2) | c1 <- ctors, c2 <- ctors ]
      mapM mkEqTypeInstance pairs
#endif
    else return [] -- the actual promotion is automatic

-- second pass through declarations to deal with type signatures
-- returns the new declarations and the list of names that have been
-- processed
promoteDec' :: Quasi q => PromoteTable -> Dec -> q ([Dec], [Name])
promoteDec' tab (SigD name ty) = case Map.lookup name tab of
  Nothing -> fail $ "Type declaration is missing its binding: " ++ (show name)
#if __GLASGOW_HASKELL__ >= 707
  Just (numArgs, eqns) ->
#else
  Just numArgs ->
#endif
    -- if there are no args, then use a type synonym, not a type family
    -- in the type synonym case, we ignore the type signature
    if numArgs == typeSynonymFlag then return $ ([], [name]) else do
      k <- promoteType ty
      let ks = unravel k
          (argKs, resultKs) = splitAt numArgs ks -- divide by uniformity
      resultK <- ravel resultKs -- rebuild the arrow kind
      tyvarNames <- mapM qNewName (replicate (length argKs) "a")
#if __GLASGOW_HASKELL__ >= 707
      return ([ClosedTypeFamilyD (promoteValName name)
                                 (zipWith KindedTV tyvarNames argKs)
                                 (Just resultK)
                                 eqns], [name])
#else
      return ([FamilyD TypeFam
                       (promoteValName name)
                       (zipWith KindedTV tyvarNames argKs)
                       (Just resultK)], [name])
#endif
    where unravel :: Kind -> [Kind] -- get argument kinds from an arrow kind
          unravel (AppT (AppT ArrowT k1) k2) =
            let ks = unravel k2 in k1 : ks
          unravel k = [k]

          ravel :: Quasi q => [Kind] -> q Kind
          ravel [] = fail "Internal error: raveling nil"
          ravel [k] = return k
          ravel (h:t) = do
            k <- ravel t
            return $ (AppT (AppT ArrowT h) k)
promoteDec' _ _ = return ([], [])

#if __GLASGOW_HASKELL__ >= 707
promoteClause :: Quasi q => TypeTable -> Name -> Clause -> QWithDecs q TySynEqn
#else
promoteClause :: Quasi q => TypeTable -> Name -> Clause -> QWithDecs q Dec
#endif
promoteClause vars _name (Clause pats body []) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  (types, vartbl) <- evalForPair $ mapM promotePat pats
  let vars' = Map.union vars vartbl
  ty <- promoteBody vars' body
#if __GLASGOW_HASKELL__ >= 707
  return $ TySynEqn types ty
#else
  return $ TySynInstD _name types ty
#endif
promoteClause _ _ (Clause _ _ (_:_)) =
  fail "A <<where>> clause in a function definition is not yet supported"

-- the LHS of a top-level expression is a name and "type with hole"
-- the hole is filled in by the RHS
data TopLevelLHS = LHS { lhsRawName :: Name -- the unpromoted name
                       , lhsName :: Name
                       , lhsHole :: Type -> Type
                       }

-- Treatment of top-level patterns is different from other patterns
-- because type families have type patterns as their LHS. However,
-- it is not possible to use type patterns at the top level, so we
-- have to use other techniques.
promoteTopLevelPat :: Quasi q => Pat -> QWithDecs q [TopLevelLHS]
promoteTopLevelPat (LitP _) = fail "Cannot declare a global literal."
promoteTopLevelPat (VarP name) = return [LHS name (promoteValName name) id]
promoteTopLevelPat (TupP pats) = case length pats of
  0 -> return [] -- unit as LHS of pattern... ignore
  1 -> fail "1-tuple encountered during top-level pattern promotion"
  n -> promoteTopLevelPat (ConP (tupleDataName n) pats)
promoteTopLevelPat (UnboxedTupP _) =
  fail "Promotion of unboxed tuples not supported"

-- to promote a constructor pattern, we need to create extraction type
-- families to pull out the individual arguments of the constructor
promoteTopLevelPat (ConP name pats) = do
  ctorInfo <- reifyWithWarning name
  (ctorType, argTypes) <- extractTypes ctorInfo
  when (length argTypes /= length pats) $
    fail $ "Inconsistent data constructor pattern: " ++ (show name) ++ " " ++
           (show pats)
  kind <- promoteType ctorType
  argKinds <- mapM promoteType argTypes
  extractorNames <- replicateM (length pats) (newUniqueName "Extract")

  varName <- qNewName "a"
  zipWithM_ (\nm arg -> addElement $ FamilyD TypeFam
                                            nm
                                            [KindedTV varName kind]
                                            (Just arg))
            extractorNames argKinds
  componentNames <- replicateM (length pats) (qNewName "a")
  zipWithM_ (\extractorName componentName ->
    addElement $ mkTyFamInst extractorName
                             [foldType (PromotedT name)
                                       (map VarT componentNames)]
                             (VarT componentName))
    extractorNames componentNames

  -- now we have the extractor families. Use the appropriate families
  -- in the "holes"
  promotedPats <- mapM promoteTopLevelPat pats
  return $ concat $
    zipWith (\lhslist extractor ->
               map (\(LHS raw nm hole) -> LHS raw nm
                                              (hole . (AppT (ConT extractor))))
                   lhslist)
            promotedPats extractorNames
  where extractTypes :: Quasi q => Info -> q (Type, [Type])
        extractTypes (DataConI datacon _dataconTy tyname _fixity) = do
          tyinfo <- reifyWithWarning tyname
          extractTypesHelper datacon tyinfo
        extractTypes _ = fail "Internal error: unexpected Info in extractTypes"

        extractTypesHelper :: Quasi q => Name -> Info -> q (Type, [Type])
        extractTypesHelper datacon
                           (TyConI (DataD _cxt tyname tvbs cons _derivs)) =
          let mcon = find ((== datacon) . fst . extractNameArgs) cons in
          case mcon of
            Nothing -> fail $ "Internal error reifying " ++ (show datacon)
            Just con -> return (foldType (ConT tyname)
                                         (map (VarT . extractTvbName) tvbs),
                                extractConArgs con)
        extractTypesHelper datacon
                           (TyConI (NewtypeD cxt tyname tvbs con derivs)) =
          extractTypesHelper datacon (TyConI (DataD cxt tyname tvbs [con] derivs))
        extractTypesHelper datacon _ =
          fail $ "Cannot promote data constructor " ++ (show datacon)

        extractConArgs :: Con -> [Type]
        extractConArgs = ctor1Case (\_ tys -> tys)
promoteTopLevelPat (InfixP l name r) = promoteTopLevelPat (ConP name [l, r])
promoteTopLevelPat (UInfixP _ _ _) =
  fail "Unresolved infix constructors not supported"
promoteTopLevelPat (ParensP _) =
  fail "Unresolved infix constructors not supported"
promoteTopLevelPat (TildeP pat) = do
  qReportWarning "Lazy pattern converted into regular pattern in promotion"
  promoteTopLevelPat pat
promoteTopLevelPat (BangP pat) = do
  qReportWarning "Strict pattern converted into regular pattern in promotion"
  promoteTopLevelPat pat
promoteTopLevelPat (AsP _name _pat) =
  fail "Promotion of aliased patterns at top level not yet supported"
promoteTopLevelPat WildP = return []
promoteTopLevelPat (RecP _ _) =
  fail "Promotion of record patterns at top level not yet supported"

-- must do a similar trick as what is in the ConP case, but this is easier
-- because Lib defined Head and Tail
promoteTopLevelPat (ListP pats) = do
  promotedPats <- mapM promoteTopLevelPat pats
  return $ concat $ snd $
    mapAccumL (\extractFn lhss ->
                 ((AppT tailTyFam) . extractFn,
                  map (\(LHS raw nm hole) ->
                         LHS raw nm (hole . (AppT headTyFam) . extractFn)) lhss))
              id promotedPats
promoteTopLevelPat (SigP pat _) = do
  qReportWarning $ "Promotion of explicit type annotation in pattern " ++
                         "not yet supported."
  promoteTopLevelPat pat
promoteTopLevelPat (ViewP _ _) =
  fail "Promotion of view patterns not yet supported"

type TypesQ q = QWithAux TypeTable q

-- promotes a term pattern into a type pattern, accumulating variable
-- binding in the auxiliary TypeTable
promotePat :: Quasi q => Pat -> TypesQ q Type
promotePat (LitP lit) = promoteLit lit
promotePat (VarP name) = do
  tyVar <- qNewName (nameBase name)
  addBinding name (VarT tyVar)
  return $ VarT tyVar
promotePat (TupP pats) = do
  types <- mapM promotePat pats
  let baseTup = PromotedTupleT (length types)
      tup = foldType baseTup types
  return tup
promotePat (UnboxedTupP _) = fail "Unboxed tuples not supported"
promotePat (ConP name pats) = do
  types <- mapM promotePat pats
  let tyCon = foldType (PromotedT name) types
  return tyCon
promotePat (InfixP pat1 name pat2) = promotePat (ConP name [pat1, pat2])
promotePat (UInfixP _ _ _) = fail "Unresolved infix constructions not supported"
promotePat (ParensP _) = fail "Unresolved infix constructions not supported"
promotePat (TildeP pat) = do
  qReportWarning "Lazy pattern converted into regular pattern in promotion"
  promotePat pat
promotePat (BangP pat) = do
  qReportWarning "Strict pattern converted into regular pattern in promotion"
  promotePat pat
promotePat (AsP name pat) = do
  ty <- promotePat pat
  addBinding name ty
  return ty
promotePat WildP = do
  name <- qNewName "z"
  return $ VarT name
promotePat (RecP _ _) = fail "Promotion of record patterns not yet supported"
promotePat (ListP pats) = do
  types <- mapM promotePat pats
  return $ foldr (\h t -> AppT (AppT PromotedConsT h) t) PromotedNilT types
promotePat (SigP pat _) = do
  qReportWarning $ "Promotion of explicit type annotation in pattern " ++
                         "not yet supported"
  promotePat pat
promotePat (ViewP _ _) = fail "View patterns not yet supported"

-- promoting a body may produce auxiliary declarations. Accumulate these.
type QWithDecs q = QWithAux [Dec] q

promoteBody :: Quasi q => TypeTable -> Body -> QWithDecs q Type
promoteBody vars (NormalB exp) = promoteExp vars exp
promoteBody _vars (GuardedB _) =
  fail "Promoting guards in patterns not yet supported"

promoteExp :: Quasi q => TypeTable -> Exp -> QWithDecs q Type
promoteExp vars (VarE name) = case Map.lookup name vars of
  Just ty -> return ty
  Nothing -> return $ promoteVal name
promoteExp _vars (ConE name) = return $ PromotedT name
promoteExp _vars (LitE lit) = promoteLit lit
promoteExp vars (AppE exp1 exp2) = do
  ty1 <- promoteExp vars exp1
  ty2 <- promoteExp vars exp2
  return $ AppT ty1 ty2
promoteExp vars (InfixE mexp1 exp mexp2) =
  case (mexp1, mexp2) of
    (Nothing, Nothing) -> promoteExp vars exp
    (Just exp1, Nothing) -> promoteExp vars (AppE exp exp1)
    (Nothing, Just _exp2) ->
      fail "Promotion of right-only sections not yet supported"
    (Just exp1, Just exp2) -> promoteExp vars (AppE (AppE exp exp1) exp2)
promoteExp _vars (UInfixE _ _ _) =
  fail "Promotion of unresolved infix operators not supported"
promoteExp _vars (ParensE _) = fail "Promotion of unresolved parens not supported"
promoteExp _vars (LamE _pats _exp) =
  fail "Promotion of lambda expressions not yet supported"
promoteExp _vars (LamCaseE _alts) =
  fail "Promotion of lambda-case expressions not yet supported"
promoteExp vars (TupE exps) = do
  tys <- mapM (promoteExp vars) exps
  let tuple = PromotedTupleT (length tys)
      tup = foldType tuple tys
  return tup
promoteExp _vars (UnboxedTupE _) = fail "Promotion of unboxed tuples not supported"
promoteExp vars (CondE bexp texp fexp) = do
  tys <- mapM (promoteExp vars) [bexp, texp, fexp]
  return $ foldType ifTyFam tys
promoteExp _vars (MultiIfE _alts) =
  fail "Promotion of multi-way if not yet supported"
promoteExp _vars (LetE _decs _exp) =
  fail "Promotion of let statements not yet supported"
promoteExp _vars (CaseE _exp _matches) =
  fail "Promotion of case statements not yet supported"
promoteExp _vars (DoE _stmts) = fail "Promotion of do statements not supported"
promoteExp _vars (CompE _stmts) =
  fail "Promotion of list comprehensions not yet supported"
promoteExp _vars (ArithSeqE _) = fail "Promotion of ranges not supported"
promoteExp vars (ListE exps) = do
  tys <- mapM (promoteExp vars) exps
  return $ foldr (\ty lst -> AppT (AppT PromotedConsT ty) lst) PromotedNilT tys
promoteExp _vars (SigE _exp _ty) =
  fail "Promotion of explicit type annotations not yet supported"
promoteExp _vars (RecConE _name _fields) =
  fail "Promotion of record construction not yet supported"
promoteExp _vars (RecUpdE _exp _fields) =
  fail "Promotion of record updates not yet supported"

promoteLit :: Monad m => Lit -> m Type
promoteLit (IntegerL n)
  | n >= 0    = return $ LitT (NumTyLit n)
  | otherwise = fail ("Promoting negative integers not supported: " ++ (show n))
promoteLit (StringL str) = return $ LitT (StrTyLit str)
promoteLit lit =
  fail ("Only string and natural number literals can be promoted: " ++ show lit)
