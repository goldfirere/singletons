{- Data/Singletons/Promote.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains functions to promote term-level constructs to the
type level. It is an internal module to the singletons package.
-}

{-# LANGUAGE TemplateHaskell, MultiWayIf, LambdaCase, TupleSections,
             ScopedTypeVariables #-}

module Data.Singletons.Promote where

import Language.Haskell.TH hiding ( Q, cxt )
import Language.Haskell.TH.Syntax ( NameSpace(..), Quasi(..) )
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OMap.Strict as OMap
import Language.Haskell.TH.Desugar.OMap.Strict (OMap)
import qualified Language.Haskell.TH.Desugar.OSet as OSet
import Language.Haskell.TH.Desugar.OSet (OSet)
import Data.Singletons.Names
import Data.Singletons.Promote.Monad
import Data.Singletons.Promote.Eq
import Data.Singletons.Promote.Defun
import Data.Singletons.Promote.Type
import Data.Singletons.Deriving.Ord
import Data.Singletons.Deriving.Bounded
import Data.Singletons.Deriving.Enum
import Data.Singletons.Deriving.Show
import Data.Singletons.Deriving.Util
import Data.Singletons.Partition
import Data.Singletons.Util
import Data.Singletons.Syntax
import Prelude hiding (exp)
import Control.Applicative (Alternative(..))
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import qualified GHC.LanguageExtensions.Type as LangExt

-- | Generate promoted definitions from a type that is already defined.
-- This is generally only useful with classes.
genPromotions :: DsMonad q => [Name] -> q [Dec]
genPromotions names = do
  checkForRep names
  infos <- mapM reifyWithLocals names
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

-- | Generate defunctionalization symbols for existing type families.
--
-- 'genDefunSymbols' has reasonable support for type families that use
-- dependent quantification. For instance, this:
--
-- @
-- type family MyProxy k (a :: k) :: Type where
--   MyProxy k (a :: k) = Proxy a
--
-- $('genDefunSymbols' [''MyProxy])
-- @
--
-- Will generate the following defunctionalization symbols:
--
-- @
-- data MyProxySym0     :: Type  ~> k ~> Type
-- data MyProxySym1 (k  :: Type) :: k ~> Type
-- @
--
-- Note that @MyProxySym0@ is a bit more general than it ought to be, since
-- there is no dependency between the first kind (@Type@) and the second kind
-- (@k@). But this would require the ability to write something like:
--
-- @
-- data MyProxySym0 :: forall (k :: Type) ~> k ~> Type
-- @
--
-- Which currently isn't possible. So for the time being, the kind of
-- @MyProxySym0@ will be slightly more general, which means that under rare
-- circumstances, you may have to provide extra type signatures if you write
-- code which exploits the dependency in @MyProxy@'s kind.
genDefunSymbols :: DsMonad q => [Name] -> q [Dec]
genDefunSymbols names = do
  checkForRep names
  infos <- mapM (dsInfo <=< reifyWithLocals) names
  decs <- promoteMDecs [] $ concatMapM defunInfo infos
  return $ decsToTH decs

-- | Produce instances for @(==)@ (type-level equality) from the given types
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

-- | Produce instances for 'PShow' from the given types
promoteShowInstances :: DsMonad q => [Name] -> q [Dec]
promoteShowInstances = concatMapM promoteShowInstance

-- | Produce an instance for 'PShow' from the given type
promoteShowInstance :: DsMonad q => Name -> q [Dec]
promoteShowInstance = promoteInstance (mkShowInstance ForPromotion) "Show"

-- | Produce an instance for @(==)@ (type-level equality) from the given type
promoteEqInstance :: DsMonad q => Name -> q [Dec]
promoteEqInstance name = do
  (tvbs, cons) <- getDataD "I cannot make an instance of (==) for it." name
  tvbs' <- mapM dsTvb tvbs
  let data_ty = foldTypeTvbs (DConT name) tvbs'
  cons' <- concatMapM (dsCon tvbs' data_ty) cons
  kind <- promoteType (foldTypeTvbs (DConT name) tvbs')
  inst_decs <- mkEqTypeInstance kind cons'
  return $ decsToTH inst_decs

promoteInstance :: DsMonad q => DerivDesc q -> String -> Name -> q [Dec]
promoteInstance mk_inst class_name name = do
  (tvbs, cons) <- getDataD ("I cannot make an instance of " ++ class_name
                            ++ " for it.") name
  tvbs' <- mapM dsTvb tvbs
  let data_ty   = foldTypeTvbs (DConT name) tvbs'
  cons' <- concatMapM (dsCon tvbs' data_ty) cons
  let data_decl = DataDecl name tvbs' cons'
  raw_inst <- mk_inst Nothing data_ty data_decl
  decs <- promoteM_ [] $ void $
          promoteInstanceDec OMap.empty Map.empty raw_inst
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
--   type family Foo (n :: Nat) :: Bool ~> Bool where
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
  PDecs { pd_let_decs                = let_decs
        , pd_class_decs              = classes
        , pd_instance_decs           = insts
        , pd_data_decs               = datas
        , pd_ty_syn_decs             = ty_syns
        , pd_open_type_family_decs   = o_tyfams
        , pd_closed_type_family_decs = c_tyfams
        , pd_derived_eq_decs         = derived_eq_decs } <- partitionDecs decls

  defunTopLevelTypeDecls ty_syns c_tyfams o_tyfams
    -- promoteLetDecs returns LetBinds, which we don't need at top level
  _ <- promoteLetDecs noPrefix let_decs
  mapM_ promoteClassDec classes
  let orig_meth_sigs = foldMap (lde_types . cd_lde) classes
      cls_tvbs_map   = Map.fromList $ map (\cd -> (cd_name cd, cd_tvbs cd)) classes
  mapM_ (promoteInstanceDec orig_meth_sigs cls_tvbs_map) insts
  mapM_ promoteDerivedEqDec   derived_eq_decs
  promoteDataDecs datas

promoteDataDecs :: [DataDecl] -> PrM ()
promoteDataDecs data_decs = do
  rec_selectors <- concatMapM extract_rec_selectors data_decs
  _ <- promoteLetDecs noPrefix rec_selectors
  mapM_ promoteDataDec data_decs
  where
    extract_rec_selectors :: DataDecl -> PrM [DLetDec]
    extract_rec_selectors (DataDecl data_name tvbs cons) =
      let arg_ty = foldTypeTvbs (DConT data_name) tvbs
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
              | (name, _) <- OMap.assocs $ lde_defns let_dec_env
              , let proName = promoteValNameLhsPrefix prefixes name
                    sym = promoteTySym proName (length all_locals) ]
  (decs, let_dec_env') <- letBind binds $ promoteLetDecEnv prefixes let_dec_env
  emitDecs decs
  return (binds, let_dec_env' { lde_proms = OMap.fromList binds })

-- Promotion of data types to kinds is automatic (see "Giving Haskell a
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
promoteDataDec (DataDecl _name _tvbs ctors) = do
  ctorSyms <- buildDefunSymsDataD ctors
  emitDecs ctorSyms

promoteClassDec :: UClassDecl
                -> PrM AClassDecl
promoteClassDec decl@(ClassDecl { cd_name = cls_name
                                , cd_tvbs = tvbs
                                , cd_fds  = fundeps
                                , cd_atfs = atfs
                                , cd_lde  = lde@LetDecEnv
                                    { lde_defns = defaults
                                    , lde_types = meth_sigs
                                    , lde_infix = infix_decls } }) = do
  let pClsName = promoteClassName cls_name
  forallBind cls_kvs_to_bind $ do
    let meth_sigs_list = OMap.assocs meth_sigs
        meth_names     = map fst meth_sigs_list
        defaults_list  = OMap.assocs defaults
        defaults_names = map fst defaults_list
    sig_decs <- mapM (uncurry promote_sig) meth_sigs_list
    (default_decs, ann_rhss, prom_rhss)
      <- mapAndUnzip3M (promoteMethod DefaultMethods meth_sigs) defaults_list
    defunAssociatedTypeFamilies tvbs atfs

    infix_decls' <- mapMaybeM (uncurry promoteInfixDecl) $ OMap.assocs infix_decls
    cls_infix_decls <- promoteReifiedInfixDecls $ cls_name:meth_names

    -- no need to do anything to the fundeps. They work as is!
    let pro_cls_dec = DClassD [] pClsName tvbs fundeps
                              (sig_decs ++ default_decs ++ infix_decls')
    emitDecs $ pro_cls_dec:cls_infix_decls
    let defaults_list'   = zip defaults_names ann_rhss
        proms            = zip defaults_names prom_rhss
        cls_kvs_to_bind' = cls_kvs_to_bind <$ meth_sigs
    return (decl { cd_lde = lde { lde_defns     = OMap.fromList defaults_list'
                                , lde_proms     = OMap.fromList proms
                                , lde_bound_kvs = cls_kvs_to_bind' } })
  where
    cls_kvb_names, cls_tvb_names, cls_kvs_to_bind :: OSet Name
    cls_kvb_names   = foldMap (foldMap fvDType . extractTvbKind) tvbs
    cls_tvb_names   = OSet.fromList $ map extractTvbName tvbs
    cls_kvs_to_bind = cls_kvb_names `OSet.union` cls_tvb_names

    promote_sig :: Name -> DType -> PrM DDec
    promote_sig name ty = do
      let proName = promoteValNameLhs name
      (argKs, resK) <- promoteUnraveled ty
      args <- mapM (const $ qNewName "arg") argKs
      let proTvbs = zipWith DKindedTV args argKs
      m_fixity <- reifyFixityWithLocals name
      emitDecsM $ defunctionalize proName m_fixity proTvbs (Just resK)

      return $ DOpenTypeFamilyD (DTypeFamilyHead proName
                                                 proTvbs
                                                 (DKindSig resK)
                                                 Nothing)

-- returns (unpromoted method name, ALetDecRHS) pairs
promoteInstanceDec :: OMap Name DType
                      -- Class method type signatures
                   -> Map Name [DTyVarBndr]
                      -- Class header type variable (e.g., if `class C a b` is
                      -- quoted, then this will have an entry for {C |-> [a, b]})
                   -> UInstDecl -> PrM AInstDecl
promoteInstanceDec orig_meth_sigs cls_tvbs_map
                   decl@(InstDecl { id_name     = cls_name
                                  , id_arg_tys  = inst_tys
                                  , id_sigs     = inst_sigs
                                  , id_meths    = meths }) = do
  cls_tvb_names <- lookup_cls_tvb_names
  inst_kis <- mapM promoteType inst_tys
  let kvs_to_bind = foldMap fvDType inst_kis
  forallBind kvs_to_bind $ do
    let subst     = Map.fromList $ zip cls_tvb_names inst_kis
        meth_impl = InstanceMethods inst_sigs subst
    (meths', ann_rhss, _)
      <- mapAndUnzip3M (promoteMethod meth_impl orig_meth_sigs) meths
    emitDecs [DInstanceD Nothing Nothing [] (foldType (DConT pClsName)
                                              inst_kis) meths']
    return (decl { id_meths = zip (map fst meths) ann_rhss })
  where
    pClsName = promoteClassName cls_name

    lookup_cls_tvb_names :: PrM [Name]
    lookup_cls_tvb_names =
      -- First, try consulting the map of class names to their type variables.
      -- It is important to do this first to ensure that we consider locally
      -- declared classes before imported ones. See #410 for what happens if
      -- you don't.
      case Map.lookup cls_name cls_tvbs_map of
        Just tvb_names -> pure $ map extractTvbName tvb_names
        Nothing -> reify_cls_tvb_names
          -- If the class isn't present in this map, we try reifying the class
          -- as a last resort.

    reify_cls_tvb_names :: PrM [Name]
    reify_cls_tvb_names = do
      let mk_tvb_names = extract_tvb_names (dsReifyTypeNameInfo pClsName)
                     <|> extract_tvb_names (dsReifyTypeNameInfo cls_name)
                      -- See Note [Using dsReifyTypeNameInfo when promoting instances]
      mb_tvb_names <- runMaybeT mk_tvb_names
      case mb_tvb_names of
        Just tvb_names -> pure tvb_names
        Nothing -> fail $ "Cannot find class declaration annotation for " ++ show cls_name

    extract_tvb_names :: PrM (Maybe DInfo) -> MaybeT PrM [Name]
    extract_tvb_names reify_info = do
      mb_info <- lift reify_info
      case mb_info of
        Just (DTyConI (DClassD _ _ tvbs _ _) _)
          -> pure $ map extractTvbName tvbs
        _ -> empty

{-
Note [Using dsReifyTypeNameInfo when promoting instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During the promotion of a class instance, it becomes necessary to reify the
original promoted class's info to learn various things. It's tempting to think
that just calling dsReify on the class name will be sufficient, but it's not.
Consider this class and its promotion:

  class Eq a where
    (==) :: a -> a -> Bool

  class PEq a where
    type (==) (x :: a) (y :: a) :: Bool

Notice how both of these classes have an identifier named (==), one at the
value level, and one at the type level. Now imagine what happens when you
attempt to promote this Template Haskell declaration:

   [d| f :: Bool
       f = () == () |]

When promoting ==, singletons will come up with its promoted equivalent (which also
happens to be ==). However, this promoted name is a raw Name, since it is created
with mkName. This becomes an issue when we call dsReify the raw "==" Name, as
Template Haskell has to arbitrarily choose between reifying the info for the
value-level (==) and the type-level (==), and in this case, it happens to pick the
value-level (==) info. We want the type-level (==) info, however, because we care
about the promoted version of (==).

Fortunately, there's a serviceable workaround. Instead of dsReify, we can use
dsReifyTypeNameInfo, which first calls lookupTypeName (to ensure we can find a Name
that's in the type namespace) and _then_ reifies it.
-}

-- Which sort of class methods are being promoted?
data MethodSort
    -- The method defaults in class declarations.
  = DefaultMethods
    -- The methods in instance declarations.
  | InstanceMethods (OMap Name DType) -- ^ InstanceSigs
                    (Map Name DKind)  -- ^ Instantiations for class tyvars
                                      --   See Note [Promoted class method kinds]
  deriving Show

promoteMethod :: MethodSort
              -> OMap Name DType    -- method types
              -> (Name, ULetDecRHS)
              -> PrM (DDec, ALetDecRHS, DType)
                 -- returns (type instance, ALetDecRHS, promoted RHS)
promoteMethod meth_sort orig_sigs_map (meth_name, meth_rhs) = do
  (meth_arg_kis, meth_res_ki) <- promote_meth_ty
  meth_arg_tvs <- mapM (const $ qNewName "a") meth_arg_kis
  let helperNameBase = case nameBase proName of
                         first:_ | not (isHsLetter first) -> "TFHelper"
                         alpha                            -> alpha

      -- family_args are the type variables in a promoted class's
      -- associated type family instance (or default implementation), e.g.,
      --
      --   class C k where
      --     type T (a :: k) (b :: Bool)
      --     type T a b = THelper1 a b        -- family_args = [a, b]
      --
      --   instance C Bool where
      --     type T a b = THelper2 a b        -- family_args = [a, b]
      --
      -- We could annotate these variables with explicit kinds, but it's not
      -- strictly necessary, as kind inference can figure them out just as well.
      family_args = map DVarT meth_arg_tvs
  helperName <- newUniqueName helperNameBase
  (pro_dec, defun_decs, ann_rhs)
    <- promoteLetDecRHS (ClassMethodRHS meth_arg_kis meth_res_ki) OMap.empty OMap.empty
                        noPrefix helperName meth_rhs
  emitDecs (pro_dec:defun_decs)
  return ( DTySynInstD
             (DTySynEqn Nothing
                        (foldType (DConT proName) family_args)
                        (foldApply (promoteValRhs helperName) (map DVarT meth_arg_tvs)))
         , ann_rhs
         , DConT (promoteTySym helperName 0) )
  where
    proName = promoteValNameLhs meth_name

    -- Promote the type of a class method. For a default method, "the type" is
    -- simply the type of the original method. For an instance method,
    -- "the type" is like the type of the original method, but substituted for
    -- the types in the instance head. (e.g., if you have `class C a` and
    -- `instance C T`, then the substitution [a |-> T] must be applied to the
    -- original method's type.)
    promote_meth_ty :: PrM ([DKind], DKind)
    promote_meth_ty =
      case meth_sort of
        DefaultMethods ->
          -- No substitution for class variables is required for default
          -- method type signatures, as they share type variables with the
          -- class they inhabit.
          lookup_meth_ty
        InstanceMethods inst_sigs_map cls_subst ->
          case OMap.lookup meth_name inst_sigs_map of
            Just ty ->
              -- We have an InstanceSig. These are easy: we can just use the
              -- instance signature's type directly, and no substitution for
              -- class variables is required.
              promoteUnraveled ty
            Nothing -> do
              -- We don't have an InstanceSig, so we must compute the kind to use
              -- ourselves.
              (arg_kis, res_ki) <- lookup_meth_ty
              -- Substitute for the class variables in the method's type.
              -- See Note [Promoted class method kinds]
              let arg_kis' = map (substKind cls_subst) arg_kis
                  res_ki'  = substKind cls_subst res_ki
              pure (arg_kis', res_ki')

    -- Attempt to look up a class method's original type.
    lookup_meth_ty :: PrM ([DKind], DKind)
    lookup_meth_ty =
      case OMap.lookup meth_name orig_sigs_map of
        Just ty ->
          -- The type of the method is in scope, so promote that.
          promoteUnraveled ty
        Nothing -> do
          -- If the type of the method is not in scope, the only other option
          -- is to try reifying the promoted method name.
          mb_info <- dsReifyTypeNameInfo proName
                     -- See Note [Using dsReifyTypeNameInfo when promoting instances]
          case mb_info of
            Just (DTyConI (DOpenTypeFamilyD (DTypeFamilyHead _ tvbs mb_res_ki _)) _)
              -> let arg_kis = map (defaultToTypeKind . extractTvbKind) tvbs
                     res_ki  = defaultToTypeKind (resultSigToMaybeKind mb_res_ki)
                  in pure (arg_kis, res_ki)
            _ -> fail $ "Cannot find type annotation for " ++ show proName

{-
Note [Promoted class method kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example of a type class (and instance):

  class C a where
    m :: a -> Bool -> Bool
    m _ x = x

  instance C [a] where
    m l _ = null l

The promoted version of these declarations would be:

  class PC a where
    type M (x :: a) (y :: Bool) (z :: Bool)
    type M x y z = MHelper1 x y z

  instance PC [a] where
    type M x y z = MHelper2 x y z

  type family MHelper1 (x :: a)   (y :: Bool) (z :: Bool) where ...
  type family MHelper2 (x :: [a]) (y :: Bool) (z :: Bool) where ...

Getting the kind signature for MHelper1 (the promoted default implementation of
M) is quite simple, as it corresponds exactly to the kind of M. We might even
choose to make that the kind of MHelper2, but then it would be overly general
(and more difficult to find in -ddump-splices output). For this reason, we
substitute in the kinds of the instance itself to determine the kinds of
promoted method implementations like MHelper2.
-}

promoteLetDecEnv :: (String, String) -> ULetDecEnv -> PrM ([DDec], ALetDecEnv)
promoteLetDecEnv prefixes (LetDecEnv { lde_defns = value_env
                                     , lde_types = type_env
                                     , lde_infix = fix_env }) = do
  infix_decls <- mapMaybeM (uncurry promoteInfixDecl) $ OMap.assocs fix_env

    -- promote all the declarations, producing annotated declarations
  let (names, rhss) = unzip $ OMap.assocs value_env
  (pro_decs, defun_decss, ann_rhss)
    <- fmap unzip3 $
       zipWithM (promoteLetDecRHS LetBindingRHS type_env fix_env prefixes) names rhss

  emitDecs $ concat defun_decss
  bound_kvs <- allBoundKindVars
  let decs = pro_decs ++ infix_decls

    -- build the ALetDecEnv
  let let_dec_env' = LetDecEnv { lde_defns     = OMap.fromList $ zip names ann_rhss
                               , lde_types     = type_env
                               , lde_infix     = fix_env
                               , lde_proms     = OMap.empty  -- filled in promoteLetDecs
                               , lde_bound_kvs = OMap.fromList $ map (, bound_kvs) names }

  return (decs, let_dec_env')

-- Promote a fixity declaration.
promoteInfixDecl :: forall q. DsMonad q => Name -> Fixity -> q (Maybe DDec)
promoteInfixDecl name fixity = do
  mb_ns <- reifyNameSpace name
  case mb_ns of
    -- If we can't find the Name for some odd reason,
    -- fall back to promote_infix_val
    Nothing        -> promote_infix_val
    Just VarName   -> promote_infix_val
    Just DataName  -> never_mind
    Just TcClsName -> do
      mb_info <- dsReify name
      case mb_info of
        Just (DTyConI DClassD{} _)
          -> finish $ promoteClassName name
        _ -> never_mind
  where
    -- Produce the fixity declaration.
    finish :: Name -> q (Maybe DDec)
    finish = pure . Just . DLetDec . DInfixD fixity

    -- Don't produce a fixity declaration at all. This happens when promoting a
    -- fixity declaration for a name whose promoted counterpart is the same as
    -- the original name.
    -- See [singletons and fixity declarations] in D.S.Single.Fixity, wrinkle 1.
    never_mind :: q (Maybe DDec)
    never_mind = pure Nothing

    -- Certain function names do not change when promoted (e.g., infix names),
    -- so don't bother with them.
    promote_infix_val :: q (Maybe DDec)
    promote_infix_val
      | nameBase name == nameBase promoted_name
      = never_mind
      | otherwise
      = finish promoted_name

    promoted_name :: Name
    promoted_name = promoteValNameLhs name

-- Try producing promoted fixity declarations for Names by reifying them
-- /without/ consulting quoted declarations. If reification fails, recover and
-- return the empty list.
-- See [singletons and fixity declarations] in D.S.Single.Fixity, wrinkle 2.
promoteReifiedInfixDecls :: forall q. DsMonad q => [Name] -> q [DDec]
promoteReifiedInfixDecls = mapMaybeM tryPromoteFixityDeclaration
  where
    tryPromoteFixityDeclaration :: Name -> q (Maybe DDec)
    tryPromoteFixityDeclaration name =
      qRecover (return Nothing) $ do
        mFixity <- qReifyFixity name
        case mFixity of
          Nothing     -> pure Nothing
          Just fixity -> promoteInfixDecl name fixity

-- Which sort of let-bound declaration's right-hand side is being promoted?
data LetDecRHSSort
    -- An ordinary (i.e., non-class-related) let-bound declaration.
  = LetBindingRHS
    -- The right-hand side of a class method (either a default method or a
    -- method in an instance declaration).
  | ClassMethodRHS [DKind] DKind -- The RHS's promoted argument and result types.
                                 -- Needed to fix #136.
  deriving Show

-- This function is used both to promote class method defaults and normal
-- let bindings. Thus, it can't quite do all the work locally and returns
-- an intermediate structure. Perhaps a better design is available.
promoteLetDecRHS :: LetDecRHSSort
                 -> OMap Name DType      -- local type env't
                 -> OMap Name Fixity     -- local fixity env't
                 -> (String, String)     -- let-binding prefixes
                 -> Name                 -- name of the thing being promoted
                 -> ULetDecRHS           -- body of the thing
                 -> PrM ( DDec          -- "type family"
                        , [DDec]        -- defunctionalization
                        , ALetDecRHS )  -- annotated RHS
promoteLetDecRHS rhs_sort type_env fix_env prefixes name (UValue exp) = do
  (res_kind, num_arrows)
    <- case rhs_sort of
         ClassMethodRHS arg_kis res_ki
           -> return ( Just (ravelTyFun (arg_kis ++ [res_ki]))
                     , length arg_kis )
         LetBindingRHS
           |  Just ty <- OMap.lookup name type_env
           -> do ki <- promoteType ty
                 return (Just ki, countArgs ty)
           |  otherwise
           -> return (Nothing, 0)
  case num_arrows of
    0 -> do
      all_locals <- allLocals
      let lde_kvs_to_bind = foldMap fvDType res_kind
      (exp', ann_exp) <- forallBind lde_kvs_to_bind $ promoteExp exp
      let proName = promoteValNameLhsPrefix prefixes name
          m_fixity = OMap.lookup name fix_env
          tvbs = map DPlainTV all_locals
      defuns <- defunctionalize proName m_fixity tvbs res_kind
      return ( DClosedTypeFamilyD
                 (DTypeFamilyHead proName tvbs (maybeKindToResultSig res_kind) Nothing)
                 [DTySynEqn Nothing (foldType (DConT proName) $ map DVarT all_locals) exp']
             , defuns
             , AValue (foldType (DConT proName) (map DVarT all_locals))
                      num_arrows ann_exp )
    _ -> do
      names <- replicateM num_arrows (newUniqueName "a")
      let pats    = map DVarP names
          newArgs = map DVarE names
      promoteLetDecRHS rhs_sort type_env fix_env prefixes name
                       (UFunction [DClause pats (foldExp exp newArgs)])

promoteLetDecRHS rhs_sort type_env fix_env prefixes name (UFunction clauses) = do
  numArgs <- count_args clauses
  (m_argKs, m_resK, ty_num_args) <- case rhs_sort of
    ClassMethodRHS arg_kis res_ki
      -> return (map Just arg_kis, Just res_ki, length arg_kis)
    LetBindingRHS
      |  Just ty <- OMap.lookup name type_env
      -> do
      -- promoteType turns arrows into TyFun. So, we unravel first to
      -- avoid this behavior. Note the use of ravelTyFun in resultK
      -- to make the return kind work out
      (argKs, resultK) <- promoteUnraveled ty
      -- invariant: count_args ty == length argKs
      return (map Just argKs, Just resultK, length argKs)

      |  otherwise
      -> return (replicate numArgs Nothing, Nothing, numArgs)
  let proName  = promoteValNameLhsPrefix prefixes name
      m_fixity = OMap.lookup name fix_env
  all_locals <- allLocals
  let local_tvbs = map DPlainTV all_locals
  tyvarNames <- mapM (const $ qNewName "a") m_argKs
  let args     = zipWith inferMaybeKindTV tyvarNames m_argKs
      all_args = local_tvbs ++ args
  defun_decs <- defunctionalize proName m_fixity all_args m_resK
  expClauses <- mapM (etaContractOrExpand ty_num_args numArgs) clauses
  let lde_kvs_to_bind = foldMap (foldMap fvDType) m_argKs <> foldMap fvDType m_resK
  (eqns, ann_clauses) <- forallBind lde_kvs_to_bind $
                         mapAndUnzipM (promoteClause proName) expClauses
  prom_fun <- lookupVarE name
  return ( DClosedTypeFamilyD
             (DTypeFamilyHead proName all_args (maybeKindToResultSig m_resK) Nothing)
             eqns
         , defun_decs
         , AFunction prom_fun ty_num_args ann_clauses )

  where
    etaContractOrExpand :: Int -> Int -> DClause -> PrM DClause
    etaContractOrExpand ty_num_args clause_num_args (DClause pats exp)
      | n >= 0 = do -- Eta-expand
          names <- replicateM n (newUniqueName "a")
          let newPats = map DVarP names
              newArgs = map DVarE names
          return $ DClause (pats ++ newPats) (foldExp exp newArgs)
      | otherwise = do -- Eta-contract
          let (clausePats, lamPats) = splitAt ty_num_args pats
          lamExp <- mkDLamEFromDPats lamPats exp
          return $ DClause clausePats lamExp
      where
        n = ty_num_args - clause_num_args

    count_args (DClause pats _ : _) = return $ length pats
    count_args _ = fail $ "Impossible! A function without clauses."

promoteClause :: Name -> DClause -> PrM (DTySynEqn, ADClause)
promoteClause proName (DClause pats exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  ((types, pats'), prom_pat_infos) <- evalForPair $ mapAndUnzipM promotePat pats
  let PromDPatInfos { prom_dpat_vars    = new_vars
                    , prom_dpat_sig_kvs = sig_kvs } = prom_pat_infos
  (ty, ann_exp) <- forallBind sig_kvs $
                   lambdaBind new_vars $
                   promoteExp exp
  all_locals <- allLocals   -- these are bound *outside* of this clause
  return ( DTySynEqn Nothing (foldType (DConT proName) $ map DVarT all_locals ++ types) ty
         , ADClause new_vars pats' ann_exp )

promoteMatch :: Name -> DMatch -> PrM (DTySynEqn, ADMatch)
promoteMatch caseTFName (DMatch pat exp) = do
  -- promoting the patterns creates variable bindings. These are passed
  -- to the function promoted the RHS
  ((ty, pat'), prom_pat_infos) <- evalForPair $ promotePat pat
  let PromDPatInfos { prom_dpat_vars    = new_vars
                    , prom_dpat_sig_kvs = sig_kvs } = prom_pat_infos
  (rhs, ann_exp) <- forallBind sig_kvs $
                    lambdaBind new_vars $
                    promoteExp exp
  all_locals <- allLocals
  return $ ( DTySynEqn Nothing
                       (foldType (DConT caseTFName) $ map DVarT all_locals ++ [ty])
                       rhs
           , ADMatch new_vars pat' ann_exp)

-- promotes a term pattern into a type pattern, accumulating bound variable names
promotePat :: DPat -> QWithAux PromDPatInfos PrM (DType, ADPat)
promotePat (DLitP lit) = (, ADLitP lit) <$> promoteLitPat lit
promotePat (DVarP name) = do
      -- term vars can be symbols... type vars can't!
  tyName <- mkTyName name
  tell $ PromDPatInfos [(name, tyName)] OSet.empty
  return (DVarT tyName, ADVarP name)
promotePat (DConP name pats) = do
  (types, pats') <- mapAndUnzipM promotePat pats
  let name' = unboxed_tuple_to_tuple name
  return (foldType (DConT name') types, ADConP name pats')
  where
    unboxed_tuple_to_tuple n
      | Just deg <- unboxedTupleNameDegree_maybe n = tupleDataName deg
      | otherwise                                  = n
promotePat (DTildeP pat) = do
  qReportWarning "Lazy pattern converted into regular pattern in promotion"
  second ADTildeP <$> promotePat pat
promotePat (DBangP pat) = do
  qReportWarning "Strict pattern converted into regular pattern in promotion"
  second ADBangP <$> promotePat pat
promotePat (DSigP pat ty) = do
  -- We must maintain the invariant that any promoted pattern signature must
  -- not have any wildcards in the underlying pattern.
  -- See Note [Singling pattern signatures].
  wildless_pat <- removeWilds pat
  (promoted, pat') <- promotePat wildless_pat
  ki <- promoteType ty
  tell $ PromDPatInfos [] (fvDType ki)
  return (DSigT promoted ki, ADSigP promoted pat' ki)
promotePat DWildP = return (DWildCardT, ADWildP)

promoteExp :: DExp -> PrM (DType, ADExp)
promoteExp (DVarE name) = fmap (, ADVarE name) $ lookupVarE name
promoteExp (DConE name) = return $ (promoteValRhs name, ADConE name)
promoteExp (DLitE lit)  = fmap (, ADLitE lit) $ promoteLitExp lit
promoteExp (DAppE exp1 exp2) = do
  (exp1', ann_exp1) <- promoteExp exp1
  (exp2', ann_exp2) <- promoteExp exp2
  return (apply exp1' exp2', ADAppE ann_exp1 ann_exp2)
-- Until we get visible kind applications, this is the best we can do.
promoteExp (DAppTypeE exp _) = do
  qReportWarning "Visible type applications are ignored by `singletons`."
  promoteExp exp
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
                               [DTySynEqn Nothing
                                          (foldType (DConT lambdaName) $
                                           map DVarT (all_locals ++ tyNames))
                                          rhs]]
  emitDecsM $ defunctionalize lambdaName Nothing tvbs Nothing
  let promLambda = foldl apply (DConT (promoteTySym lambdaName 0))
                               (map DVarT all_locals)
  return (promLambda, ADLamE tyNames promLambda names ann_exp)
promoteExp (DCaseE exp matches) = do
  caseTFName <- newUniqueName "Case"
  all_locals <- allLocals
  let prom_case = foldType (DConT caseTFName) (map DVarT all_locals)
  (exp', ann_exp)     <- promoteExp exp
  (eqns, ann_matches) <- mapAndUnzipM (promoteMatch caseTFName) matches
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
  let letPrefixes = uniquePrefixes "Let" "<<<" unique
  (binds, ann_env) <- promoteLetDecs letPrefixes decs
  (exp', ann_exp) <- letBind binds $ promoteExp exp
  return (exp', ADLetE ann_env ann_exp)
promoteExp (DSigE exp ty) = do
  (exp', ann_exp) <- promoteExp exp
  ty' <- promoteType ty
  return (DSigT exp' ty', ADSigE exp' ann_exp ty')
promoteExp e@(DStaticE _) = fail ("Static expressions cannot be promoted: " ++ show e)

promoteLitExp :: Quasi q => Lit -> q DType
promoteLitExp (IntegerL n)
  | n >= 0    = return $ (DConT tyFromIntegerName `DAppT` DLitT (NumTyLit n))
  | otherwise = return $ (DConT tyNegateName `DAppT`
                          (DConT tyFromIntegerName `DAppT` DLitT (NumTyLit (-n))))
promoteLitExp (StringL str) = do
  let prom_str_lit = DLitT (StrTyLit str)
  os_enabled <- qIsExtEnabled LangExt.OverloadedStrings
  pure $ if os_enabled
         then DConT tyFromStringName `DAppT` prom_str_lit
         else prom_str_lit
promoteLitExp lit =
  fail ("Only string and natural number literals can be promoted: " ++ show lit)

promoteLitPat :: MonadFail m => Lit -> m DType
promoteLitPat (IntegerL n)
  | n >= 0    = return $ (DLitT (NumTyLit n))
  | otherwise =
    fail $ "Negative literal patterns are not allowed,\n" ++
           "because literal patterns are promoted to natural numbers."
promoteLitPat (StringL str) = return $ DLitT (StrTyLit str)
promoteLitPat lit =
  fail ("Only string and natural number literals can be promoted: " ++ show lit)

-- See Note [DerivedDecl]
promoteDerivedEqDec :: DerivedEqDecl -> PrM ()
promoteDerivedEqDec (DerivedDecl { ded_type = ty
                                 , ded_decl = DataDecl _ _ cons }) = do
  kind <- promoteType ty
  inst_decs <- mkEqTypeInstance kind cons
  emitDecs inst_decs
