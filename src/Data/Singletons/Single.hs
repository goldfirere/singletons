{- Data/Singletons/Single.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains functions to refine constructs to work with singleton
types. It is an internal module to the singletons package.
-}
{-# LANGUAGE TemplateHaskell, TupleSections, ParallelListComp #-}

module Data.Singletons.Single where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (Quasi(..))
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons.Promote.Monad ( promoteM, promoteM_ )
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Type
import Data.Singletons.Single.Data
import Data.Singletons.Single.Eq
import Data.Singletons.Syntax
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Sweeten
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import Control.Monad
import Control.Applicative

{-
How singletons works
~~~~~~~~~~~~~~~~~~~~

Singling, on the surface, doesn't seem all that complicated. Promote the type,
and singletonize all the terms. That's essentially what was done singletons < 1.0.
But, now we want to deal with higher-order singletons. So, things are a little
more complicated.

The way to understand all of this is that *every* variable maps to something
of type (Sing t), for an appropriately-kinded t. This includes functions, which
use the "SLambda" instance of Sing. To apply singleton functions, we use the
applySing function.

That, in an of itself, wouldn't be too hard, but it's really annoying from
the user standpoint. After dutifully singling `map`, a user doesn't want to
have to use two `applySing`s to actually use it. So, any let-bound identifier
is eta-expanded so that the singled type has the same number of arrows as
the original type. (If there is no original type signature, then it has as
many arrows as the original had patterns.) Then, we store a use of one of the
singFunX functions in the SgM environment so that every use of a let-bound
identifier has a proper type (Sing t).

It would be consistent to avoid this eta-expansion for local lets (as opposed
to top-level lets), but that seemed like more bother than it was worth. It
may also be possible to be cleverer about nested eta-expansions and contractions,
but that also seemed not to be worth it. Though I haven't tested it, my hope
is that the eta-expansions and contractions have no runtime effect, especially
because SLambda is a *newtype* instance, not a *data* instance.

Note that to maintain the desired invariant, we must also be careful to eta-
contract constructors. This is the point of buildLets.
-}

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
singDecideInstances :: Quasi q => [Name] -> q [Dec]
singDecideInstances = concatMapM singDecideInstance

-- | Create instance of 'SDecide' for the given type.
singDecideInstance :: Quasi q => Name -> q [Dec]
singDecideInstance name = singEqualityInstance sDecideClassDesc name

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

singTopLevelDecs :: Quasi q => [DDec] -> q [DDec]
singTopLevelDecs decls = do
  PDecs { pd_let_decs              = letDecls
        , pd_class_decs            = classes
        , pd_instance_decs         = insts
        , pd_data_decs             = datas }    <- partitionDecs decls

  when (not (null classes) || not (null insts)) $
    qReportError "Classes and instances may not yet be made into singletons."

  dataDecls' <- promoteM_ $ promoteDataDecs datas
  ((_, letDecEnv), letDecls') <- promoteM $ promoteLetDecs noPrefix letDecls
  singDecsM $ do
    let letBinds = concatMap buildDataLets datas
                ++ concatMap buildMethLets classes
    (newLetDecls, newDataDecls) <- bindLets letBinds $
                                   singLetDecEnv TopLevel letDecEnv $
                                   concatMapM singDataD datas
    return $ dataDecls' ++ letDecls' ++ (map DLetDec newLetDecls) ++ newDataDecls

-- see comment at top of file
buildDataLets :: DataDecl -> [(Name, DExp)]
buildDataLets (DataDecl _nd _name _tvbs cons _derivings) =
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

buildMethLets :: ClassDecl -> [(Name, DExp)]
buildMethLets = error "Cannot singletonize class definitions yet."
  -- FIXME!

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
        Just (AValue _ n _) -> return n
        Just (AFunction _ n _) -> return n

      -- create a Sing t1 -> Sing t2 -> ... type of a given arity and result type
    mk_sing_ty :: Int -> DType -> SgM (DType, [Name])
    mk_sing_ty n prom_ty = do
      arg_names <- replicateM n (qNewName "arg")
      return ( DForallT (map DPlainTV arg_names) []
                        (ravel (map (\name -> singFamily `DAppT` DVarT name) arg_names
                                ++ [singFamily `DAppT`
                                    (foldl apply prom_ty (map DVarT arg_names))]))
             , arg_names )

    sing_let_dec :: Map Name [Name] -> Name -> ALetDecRHS -> SgM DLetDec
    sing_let_dec _bound_names name (AValue prom num_arrows exp) =
      DValD (DVarPa (singValName name)) <$>
      (wrapUnSingFun num_arrows prom <$> singExp exp)
    sing_let_dec bound_names name (AFunction prom_fun num_arrows clauses) =
      let tyvar_names = case Map.lookup name bound_names of
                          Nothing -> []
                          Just ns -> ns
      in
      DFunD (singValName name) <$> mapM (singClause prom_fun num_arrows tyvar_names) clauses

singClause :: DType   -- the promoted function
           -> Int     -- the number of arrows in the type. If this is more
                      -- than the number of patterns, we need to eta-expand
                      -- with unSingFun.
           -> [Name]  -- the names of the forall'd vars in the type sig of this
                      -- function. This list should have at least the length as the
                      -- number of patterns in the clause
           -> ADClause -> SgM DClause
singClause prom_fun num_arrows bound_names (ADClause var_proms pats exp) = do
  ((sPats, prom_pats), wilds)
    <- evalForPair $ mapAndUnzipM (singPat (Map.fromList var_proms) Parameter) pats
  let equalities = zip (map DVarT bound_names) prom_pats
      applied_ty = foldl apply prom_fun prom_pats
  sBody <- bindTyVarsClause var_proms wilds applied_ty equalities $ singExp exp
    -- when calling unSingFun, the prom_pats aren't in scope, so we use the
    -- bound_names instead
  let pattern_bound_names = zipWith const bound_names pats
       -- this does eta-expansion. See comment at top of file.
      sBody' = wrapUnSingFun (num_arrows - length pats)
                 (foldl apply prom_fun (map DVarT pattern_bound_names)) sBody
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
        -> PatternContext
        -> DPat
        -> QWithAux [Name]  -- these names must be forall-bound
           SgM (DPat, DType) -- the type form of the pat
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
  qReportWarning
    "Lazy pattern converted into regular pattern during singleton generation."
  singPat var_proms patCxt pat
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
