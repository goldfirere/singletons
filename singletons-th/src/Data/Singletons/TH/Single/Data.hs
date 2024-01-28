{- Data/Singletons/TH/Single/Data.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

Singletonizes constructors.
-}

module Data.Singletons.TH.Single.Data
  ( singDataD
  , singCtor
  ) where

import Language.Haskell.TH.Desugar as Desugar
import Language.Haskell.TH.Syntax
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Traversable (mapAccumL)
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Promote.Type
import Data.Singletons.TH.Single.Defun
import Data.Singletons.TH.Single.Fixity
import Data.Singletons.TH.Single.Monad
import Data.Singletons.TH.Syntax
import Data.Singletons.TH.Util
import Control.Monad

-- We wish to consider the promotion of "Rep" to be *
-- not a promoted data constructor.
singDataD :: DataDecl -> SgM [DDec]
singDataD (DataDecl df name tvbs ctors) = do
  opts <- getOptions
  let reqTvbNames   = map extractTvbName $
                      filter (\tvb -> extractTvbFlag tvb == BndrReq) tvbs
      ctor_names    = map extractName ctors
      rec_sel_names = concatMap extractRecSelNames ctors
  k <- promoteType (foldTypeTvbs (DConT name) tvbs)
  mb_data_sak <- dsReifyType name
  ctors' <- mapM (singCtor name) ctors
  fixityDecs <- singReifiedInfixDecls $ ctor_names ++ rec_sel_names
  -- instance for SingKind
  fromSingClauses     <- mapM mkFromSingClause ctors
  emptyFromSingClause <- mkEmptyFromSingClause
  toSingClauses       <- mapM mkToSingClause ctors
  emptyToSingClause   <- mkEmptyToSingClause
  let singKindInst =
        DInstanceD Nothing Nothing
                   (map (singKindConstraint . DVarT) reqTvbNames)
                   (DAppT (DConT singKindClassName) k)
                   [ DTySynInstD $ DTySynEqn Nothing
                      (DConT demoteName `DAppT` k)
                      (foldType (DConT name)
                        (map (DAppT demote . DVarT) reqTvbNames))
                   , DLetDec $ DFunD fromSingName
                               (fromSingClauses `orIfEmpty` [emptyFromSingClause])
                   , DLetDec $ DFunD toSingName
                               (toSingClauses   `orIfEmpty` [emptyToSingClause]) ]

  let singDataName = singledDataTypeName opts name
      -- e.g. type instance Sing @Nat = SNat
      singSynInst =
        DTySynInstD $ DTySynEqn Nothing
                                (DConT singFamilyName `DAppKindT` k)
                                (DConT singDataName)

      -- Note that we always include an explicit result kind in the body of the
      -- singleton data type declaration, even if it has a standalone kind
      -- signature that would make this explicit result kind redudant.
      -- See Note [Keep redundant kind information for Haddocks]
      -- in D.S.TH.Promote.
      mk_data_dec kind =
        DDataD Data [] singDataName [] (Just kind) ctors' []

  data_decs <- case mb_data_sak of
    -- No standalone kind signature. Try to figure out the order of kind
    -- variables on a best-effort basis.
    Nothing -> do
      let sing_tvbs = changeDTVFlags SpecifiedSpec $
                      toposortTyVarsOf $ map dTyVarBndrToDType tvbs
          kinded_sing_ty = DForallT (DForallInvis sing_tvbs) $
                           DArrowT `DAppT` k `DAppT` DConT typeKindName
      pure [mk_data_dec kinded_sing_ty]

    -- A standalone kind signature is provided, so use that to determine the
    -- order of kind variables.
    Just data_sak -> do
      sing_data_sak <- singDataSAK data_sak tvbs k
      pure [ DKiSigD singDataName sing_data_sak
           , mk_data_dec sing_data_sak
           ]

  return $ data_decs ++
           singSynInst :
           [ singKindInst | genSingKindInsts opts
                          , -- `type data` data constructors only exist at the
                            -- type level. As such, we cannot define SingKind
                            -- instances for them, as they require term-level
                            -- data constructors to implement.
                            df /= Desugar.TypeData
                          ] ++
           fixityDecs
  where -- in the Rep case, the names of the constructors are in the wrong scope
        -- (they're types, not datacons), so we have to reinterpret them.
        mkConName :: Name -> SgM Name
        mkConName
          | nameBase name == nameBase repName = mkDataName . nameBase
          | otherwise                         = return

        mkFromSingClause :: DCon -> SgM DClause
        mkFromSingClause c = do
          opts <- getOptions
          let (cname, numArgs) = extractNameArgs c
          cname' <- mkConName cname
          varNames <- replicateM numArgs (qNewName "b")
          return $ DClause [DConP (singledDataConName opts cname) [] (map DVarP varNames)]
                           (foldExp
                              (DConE cname')
                              (map (DAppE (DVarE fromSingName) . DVarE) varNames))

        mkToSingClause :: DCon -> SgM DClause
        mkToSingClause (DCon _tvbs _cxt cname fields _rty) = do
          opts <- getOptions
          let types = tysOfConFields fields
          varNames  <- mapM (const $ qNewName "b") types
          svarNames <- mapM (const $ qNewName "c") types
          promoted  <- mapM promoteType types
          cname' <- mkConName cname
          let varPats        = zipWith mkToSingVarPat varNames promoted
              recursiveCalls = zipWith mkRecursiveCall varNames promoted
          return $
            DClause [DConP cname' [] varPats]
                    (multiCase recursiveCalls
                               (map (DConP someSingDataName [] . listify . DVarP)
                                    svarNames)
                               (DAppE (DConE someSingDataName)
                                         (foldExp (DConE (singledDataConName opts cname))
                                                  (map DVarE svarNames))))

        mkToSingVarPat :: Name -> DKind -> DPat
        mkToSingVarPat varName ki =
          DSigP (DVarP varName) (DAppT (DConT demoteName) ki)

        mkRecursiveCall :: Name -> DKind -> DExp
        mkRecursiveCall var_name ki =
          DSigE (DAppE (DVarE toSingName) (DVarE var_name))
                (DAppT (DConT someSingTypeName) ki)

        mkEmptyFromSingClause :: SgM DClause
        mkEmptyFromSingClause = do
          x <- qNewName "x"
          pure $ DClause [DVarP x]
               $ DCaseE (DVarE x) []

        mkEmptyToSingClause :: SgM DClause
        mkEmptyToSingClause = do
          x <- qNewName "x"
          pure $ DClause [DVarP x]
               $ DConE someSingDataName `DAppE` DCaseE (DVarE x) []

-- Single a constructor.
singCtor :: Name -> DCon -> SgM DCon
 -- polymorphic constructors are handled just
 -- like monomorphic ones -- the polymorphism in
 -- the kind is automatic
singCtor dataName (DCon con_tvbs cxt name fields rty)
  | not (null cxt)
  = fail "Singling of constrained constructors not yet supported"
  | otherwise
  = do
  opts <- getOptions
  let types = tysOfConFields fields
      numTypes = length types
      sName = singledDataConName opts name
      sCon = DConE sName
      pCon = DConT $ promotedDataTypeOrConName opts name
  checkVanillaDType $ ravelVanillaDType con_tvbs [] types rty
  indexNames <- mapM (const $ qNewName "n") types
  kinds <- mapM promoteType_NC types
  rty' <- promoteType_NC rty
  let indices = map DVarT indexNames
      kindedIndices = zipWith DSigT indices kinds
      -- The approach we use for singling data constructor types differs
      -- slightly from the approach taken in D.S.TH.Single.Type.singType in that
      -- we always explicitly quantify all type variables in a singled data
      -- constructor, regardless of whether the original data constructor
      -- explicitly quantified them or not. This explains the use of
      -- toposortTyVarsOf below.
      -- See Note [Preserve the order of type variables during singling]
      -- (wrinkle 1) in D.S.TH.Single.Type.
      kvbs | null con_tvbs
           = changeDTVFlags SpecifiedSpec (toposortTyVarsOf (kinds ++ [rty'])) ++
             con_tvbs
           | otherwise
           = con_tvbs
      all_tvbs = kvbs ++ zipWith (`DKindedTV` SpecifiedSpec) indexNames kinds

  -- @mb_SingI_dec k@ returns 'Just' an instance of @SingI<k>@ if @k@ is
  -- less than or equal to the number of fields in the data constructor.
  -- Otherwise, it returns 'Nothing'.
  let mb_SingI_dec :: Int -> Maybe DDec
      mb_SingI_dec k
        | k <= numTypes
        = let take_until_k = take (numTypes - k) in
          Just $ DInstanceD Nothing Nothing
                   (map (DAppT (DConT singIName)) (take_until_k indices))
                   (DAppT (DConT (mkSingIName k))
                          (foldType pCon (take_until_k kindedIndices)))
                   [DLetDec $ DValD (DVarP (mkSingMethName k))
                          (foldExp sCon (replicate (numTypes - k) (DVarE singMethName)))]
        | otherwise
        = Nothing

  -- SingI instances for data constructor
  when (genSingIInsts opts) $
    emitDecs $ mapMaybe mb_SingI_dec [0, 1, 2]
  -- SingI instances for defunctionalization symbols. Note that we don't
  -- support contexts in constructors at the moment, so it's fine for now to
  -- just assume that the context is always ().
  emitDecs =<< singDefuns name DataName [] (map Just kinds) (Just rty')

  conFields <- case fields of
    DNormalC dInfix bts -> DNormalC dInfix <$>
                           zipWithM (\(b, _) index -> mk_bang_type b index)
                                    bts indices
    DRecC vbts          -> DNormalC False <$>
                           zipWithM (\(_, b, _) index -> mk_bang_type b index)
                                    vbts indices
                      -- Don't bother looking at record selectors, as they are
                      -- handled separately in singTopLevelDecs.
                      -- See Note [singletons-th and record selectors]
  return $ DCon all_tvbs [] sName conFields
                (DConT (singledDataTypeName opts dataName) `DAppT`
                  (foldType pCon indices `DSigT` rty'))
                  -- Make sure to include an explicit `rty'` kind annotation.
                  -- See Note [Preserve the order of type variables during singling],
                  -- wrinkle 3, in D.S.TH.Single.Type.
  where
    mk_source_unpackedness :: SourceUnpackedness -> SgM SourceUnpackedness
    mk_source_unpackedness su = case su of
      NoSourceUnpackedness -> pure su
      SourceNoUnpack       -> pure su
      SourceUnpack         -> do
        -- {-# UNPACK #-} is essentially useless in a singletons setting, since
        -- all singled data types are GADTs. See GHC#10016.
        qReportWarning "{-# UNPACK #-} pragmas are ignored by `singletons-th`."
        pure NoSourceUnpackedness

    mk_bang :: Bang -> SgM Bang
    mk_bang (Bang su ss) = do su' <- mk_source_unpackedness su
                              pure $ Bang su' ss

    mk_bang_type :: Bang -> DType -> SgM DBangType
    mk_bang_type b index = do b' <- mk_bang b
                              pure (b', DAppT singFamily index)

-- @'singDataSAK' sak data_bndrs@ produces a standalone kind signature for a
-- singled data declaration, using the original data type's standalone kind
-- signature (@sak@) and its user-written binders (@data_bndrs@) as a template.
-- For this example:
--
-- @
-- type D :: forall j k. k -> j -> Type
-- data D @j @l (a :: l) b = ...
-- @
--
-- We would produce the following standalone kind signature:
--
-- @
-- type SD :: forall j l (a :: l) (b :: j). D @j @l (a :: l) b -> Type
-- @
--
-- Note that:
--
-- * This function has a precondition that the length of @data_bndrs@ must
--   always be equal to the number of visible quantifiers (i.e., the number of
--   function arrows plus the number of visible @forall@–bound variables) in
--   @sak@. @singletons-th@ maintains this invariant when constructing a
--   'DataDecl' (see the 'buildDataDTvbs' function).
--
-- * The order of the invisible quantifiers is preserved, so both
--   @D \@Bool \@Ordering@ and @SD \@Bool \@Ordering@ will work the way you would
--   expect it to.
--
-- * Whenever possible, this function reuses type variable names from the data
--   type's user-written binders. This is why the standalone kind signature uses
--   @forall j l@ instead of @forall j k@, since the @(a :: l)@ binder uses @l@
--   instead of @k@. We could have just as well chose the other way around, but
--   we chose to pick variable names from the data type binders since they scope
--   over other parts of the data type declaration (e.g., in @deriving@
--   clauses), so keeping these names avoids having to perform some
--   alpha-renaming.
singDataSAK ::
     MonadFail q
  => DKind
     -- ^ The standalone kind signature for the original data type
  -> [DTyVarBndrVis]
     -- ^ The user-written binders for the original data type
  -> DKind
     -- ^ The original data type, promoted to a kind
  -> q DKind
     -- ^ The standalone kind signature for the singled data type
singDataSAK data_sak data_bndrs data_k = do
  -- (1) First, explicitly quantify any free kind variables in `data_sak` using
  -- an invisible @forall@. This is done to ensure that precondition (2) in
  -- `matchUpSigWithDecl` is upheld. (See the Haddocks for that function).
  let data_sak_free_tvbs =
        changeDTVFlags SpecifiedSpec $ toposortTyVarsOf [data_sak]
      data_sak' = DForallT (DForallInvis data_sak_free_tvbs) data_sak

  -- (2) Next, compute type variable binders for the singled data type's
  -- standalone kind signature using `matchUpSigWithDecl`. Note that these can
  -- be biased towards type variable names mention in `data_sak` over names
  -- mentioned in `data_bndrs`, but we will fix that up in the next step.
  let (data_sak_args, _) = unravelDType data_sak'
  sing_sak_tvbs <- matchUpSigWithDecl data_sak_args data_bndrs

  -- (3) Swizzle the type variable names so that names in `data_bndrs` are
  -- preferred over names in `data_sak`.
  --
  -- This is heavily inspired by similar code in GHC:
  -- https://gitlab.haskell.org/ghc/ghc/-/blob/cec903899234bf9e25ea404477ba846ac1e963bb/compiler/GHC/Tc/Gen/HsType.hs#L2607-2616
  let invis_data_sak_args = filterInvisTvbArgs data_sak_args
      invis_data_sak_arg_nms = map extractTvbName invis_data_sak_args

      invis_data_bndrs = toposortKindVarsOfTvbs data_bndrs
      invis_data_bndr_nms = map extractTvbName invis_data_bndrs

      swizzle_env =
        Map.fromList $ zip invis_data_sak_arg_nms invis_data_bndr_nms
      (_, swizzled_sing_sak_tvbs) =
        mapAccumL (swizzleTvb swizzle_env) Map.empty sing_sak_tvbs

  -- (4) Finally, construct the kind of the singled data type.
  pure $ DForallT (DForallInvis swizzled_sing_sak_tvbs)
       $ DArrowT `DAppT` data_k `DAppT` DConT typeKindName

-- Match the quantifiers in a data type's standalone kind signature with the
-- binders in the data type declaration. This function assumes the following
-- preconditions:
--
-- 1. The number of required binders in the data type declaration is equal to
--    the number of visible quantifiers (i.e., the number of function arrows
--    plus the number of visible @forall@–bound variables) in the standalone
--    kind signature.
--
-- 2. The number of invisible \@-binders in the data type declaration is less
--    than or equal to the number of invisible quantifiers (i.e., the number of
--    invisible @forall@–bound variables) in the standalone kind signature.
--
-- The implementation of this function is heavily based on a GHC function of
-- the same name:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/1464a2a8de082f66ae250d63ab9d94dbe2ef8620/compiler/GHC/Tc/Gen/HsType.hs#L2645-2715
matchUpSigWithDecl ::
     forall q.
     MonadFail q
  => DFunArgs
     -- ^ The quantifiers in the data type's standalone kind signature
  -> [DTyVarBndrVis]
     -- ^ The user-written binders in the data type declaration
  -> q [DTyVarBndrSpec]
matchUpSigWithDecl = go_fun_args Map.empty
  where
    go_fun_args ::
         DSubst
         -- ^ A substitution from the names of @forall@-bound variables in the
         -- standalone kind signature to corresponding binder names in the
         -- user-written binders. (See the Haddocks for `singDataSAK` for an
         -- explanation of why we perform this substitution.) For example:
         --
         -- @
         -- type T :: forall a. forall b -> Maybe (a, b) -> Type
         -- data T @x y z
         -- @
         --
         -- After matching up the @a@ in @forall a.@ with @x@ and
         -- the @b@ in @forall b ->@ with @y@, this substitution will be
         -- extended with @[a :-> x, b :-> y]@. This ensures that we will
         -- produce @Maybe (x, y)@ instead of @Maybe (a, b)@ in
         -- the kind for @z@.
      -> DFunArgs -> [DTyVarBndrVis] -> q [DTyVarBndrSpec]
    go_fun_args _ DFANil [] =
      pure []
    -- This should not happen, per the function's precondition
    go_fun_args _ DFANil data_bndrs =
      fail $ "matchUpSigWithDecl.go_fun_args: Too many binders: " ++ show data_bndrs
    -- GHC now disallows kind-level constraints, per this GHC proposal:
    -- https://github.com/ghc-proposals/ghc-proposals/blob/b0687d96ce8007294173b7f628042ac4260cc738/proposals/0547-no-kind-equalities.rst
    go_fun_args _ (DFACxt{}) _ =
      fail "matchUpSigWithDecl.go_fun_args: Unexpected kind-level constraint"
    go_fun_args subst (DFAForalls (DForallInvis tvbs) sig_args) data_bndrs =
      go_invis_tvbs subst tvbs sig_args data_bndrs
    go_fun_args subst (DFAForalls (DForallVis tvbs) sig_args) data_bndrs =
      go_vis_tvbs subst tvbs sig_args data_bndrs
    go_fun_args subst (DFAAnon anon sig_args) (data_bndr:data_bndrs) = do
      let data_bndr_name = extractTvbName data_bndr
          mb_data_bndr_kind = extractTvbKind data_bndr
          anon' = substType subst anon

          anon'' =
            case mb_data_bndr_kind of
              Nothing -> anon'
              Just data_bndr_kind ->
                let mb_match_subst = matchTy NoIgnore data_bndr_kind anon' in
                maybe data_bndr_kind (`substType` data_bndr_kind) mb_match_subst
      sig_args' <- go_fun_args subst sig_args data_bndrs
      pure $ DKindedTV data_bndr_name SpecifiedSpec anon'' : sig_args'
    -- This should not happen, per precondition (1).
    go_fun_args _ _ [] =
      fail "matchUpSigWithDecl.go_fun_args: Too few binders"

    go_invis_tvbs :: DSubst -> [DTyVarBndrSpec] -> DFunArgs -> [DTyVarBndrVis] -> q [DTyVarBndrSpec]
    go_invis_tvbs subst [] sig_args data_bndrs =
      go_fun_args subst sig_args data_bndrs
    -- This should not happen, per precondition (2).
    go_invis_tvbs _ (_:_) _ [] =
      fail $ "matchUpSigWithDecl.go_invis_tvbs: Too few binders"
    go_invis_tvbs subst (invis_tvb:invis_tvbs) sig_args data_bndrss@(data_bndr:data_bndrs) =
      case extractTvbFlag data_bndr of
        -- If the next data_bndr is required, then we have a invisible forall in
        -- the kind without a corresponding invisible @-binder, which is
        -- allowed. In this case, we simply apply the substitution and recurse.
        BndrReq -> do
          let (subst', invis_tvb') = substTvb subst invis_tvb
          sig_args' <- go_invis_tvbs subst' invis_tvbs sig_args data_bndrss
          pure $ invis_tvb' : sig_args'
        -- If the next data_bndr is an invisible @-binder, then we must match it
        -- against the invisible forall–bound variable in the kind.
        BndrInvis -> do
          let (subst', sig_tvb) = match_tvbs subst invis_tvb data_bndr
          sig_args' <- go_invis_tvbs subst' invis_tvbs sig_args data_bndrs
          pure (sig_tvb : sig_args')

    go_vis_tvbs :: DSubst -> [DTyVarBndrUnit] -> DFunArgs -> [DTyVarBndrVis] -> q [DTyVarBndrSpec]
    go_vis_tvbs subst [] sig_args data_bndrs =
      go_fun_args subst sig_args data_bndrs
    -- This should not happen, per precondition (1).
    go_vis_tvbs _ (_:_) _ [] =
      fail $ "matchUpSigWithDecl.go_vis_tvbs: Too few binders"
    go_vis_tvbs subst (vis_tvb:vis_tvbs) sig_args (data_bndr:data_bndrs) = do
      case extractTvbFlag data_bndr of
        -- If the next data_bndr is required, then we must match it against the
        -- visible forall–bound variable in the kind.
        BndrReq -> do
          let (subst', sig_tvb) = match_tvbs subst vis_tvb data_bndr
          sig_args' <- go_vis_tvbs subst' vis_tvbs sig_args data_bndrs
          pure (sig_tvb : sig_args')
        -- We have a visible forall in the kind, but an invisible @-binder as
        -- the next data_bndr. This is ill kinded, so throw an error.
        BndrInvis ->
          fail $ "matchUpSigWithDecl.go_vis_tvbs: Expected visible binder, encountered invisible binder: "
              ++ show data_bndr

    -- @match_tvbs subst sig_tvb data_bndr@ will match the kind of @data_bndr@
    -- against the kind of @sig_tvb@ to produce a new kind. This function
    -- produces two values as output:
    --
    -- 1. A new @subst@ that has been extended such that the name of @sig_tvb@
    --    maps to the name of @data_bndr@. (See the Haddocks for the 'DSubst'
    --    argument to @go_fun_args@ for an explanation of why we do this.)
    --
    -- 2. A 'DTyVarBndrSpec' that has the name of @data_bndr@, but with the new
    --    kind resulting from matching.
    match_tvbs :: DSubst -> DTyVarBndr flag -> DTyVarBndrVis -> (DSubst, DTyVarBndrSpec)
    match_tvbs subst sig_tvb data_bndr =
      let data_bndr_name = extractTvbName data_bndr
          mb_data_bndr_kind = extractTvbKind data_bndr

          sig_tvb_name = extractTvbName sig_tvb
          mb_sig_tvb_kind = substType subst <$> extractTvbKind sig_tvb

          mb_kind :: Maybe DKind
          mb_kind =
            case (mb_data_bndr_kind, mb_sig_tvb_kind) of
              (Nothing,             Nothing)           -> Nothing
              (Just data_bndr_kind, Nothing)           -> Just data_bndr_kind
              (Nothing,             Just sig_tvb_kind) -> Just sig_tvb_kind
              (Just data_bndr_kind, Just sig_tvb_kind) -> do
                match_subst <- matchTy NoIgnore data_bndr_kind sig_tvb_kind
                Just $ substType match_subst data_bndr_kind

          subst' = Map.insert sig_tvb_name (DVarT data_bndr_name) subst
          sig_tvb' = case mb_kind of
            Nothing   -> DPlainTV  data_bndr_name SpecifiedSpec
            Just kind -> DKindedTV data_bndr_name SpecifiedSpec kind in

      (subst', sig_tvb')

-- This is heavily inspired by the `swizzleTcb` function in GHC:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/cec903899234bf9e25ea404477ba846ac1e963bb/compiler/GHC/Tc/Gen/HsType.hs#L2741-2755
swizzleTvb :: Map Name Name -> DSubst -> DTyVarBndrSpec -> (DSubst, DTyVarBndrSpec)
swizzleTvb swizzle_env subst tvb =
  (subst', tvb2)
  where
    subst' = Map.insert tvb_name (DVarT (extractTvbName tvb2)) subst
    tvb_name = extractTvbName tvb
    tvb1 = mapDTVKind (substType subst) tvb
    tvb2 =
      case Map.lookup tvb_name swizzle_env of
        Just user_name -> mapDTVName (const user_name) tvb1
        Nothing        -> tvb1

{-
Note [singletons-th and record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Record selectors are annoying to deal with in singletons-th for various reasons:

1. There is no record syntax at the type level, so promoting code that involves
   records in some way is not straightforward.
2. One can define record selectors for singled data types, but they're rife
   with peril. Some pitfalls include:

   * Singling record updates often produces code that does not typecheck. For
     example, this works:

       let i = Identity True in i { runIdentity = False }

     But this does /not/ work:

       let si = SIdentity STrue in si { sRunIdentity = SFalse }

       error:
           • Record update for insufficiently polymorphic field:
               sRunIdentity :: Sing n
           • In the expression: si {sRunIdentity = SFalse}
             In the expression:
               let si = SIdentity STrue in si {sRunIdentity = SFalse}

     Ugh. See GHC#16501.

   * Singling a data type with multiple constructors that share a record
     selector name will /also/ not typecheck. While this works:

       data X = X1 {y :: Bool} | X2 {y :: Bool}

     This does not:

       data SX :: X -> Type where
         SX1 :: { sY :: Sing n } -> SX ('X1 n)
         SY1 :: { sY :: Sing n } -> SX ('X2 n)

       error:
           • Constructors SX1 and SX2 have a common field ‘sY’,
               but have different result types
           • In the data type declaration for ‘SX’

     Double ugh. See GHC#8673/GHC#12159.

   * Even if a data type only has a single constructor with record selectors,
     singling it can induce headaches. One might be tempted to single this type:

       newtype Unit = MkUnit { runUnit :: () }

     With this code:

       data SUnit :: Unit -> Type where
         SMkUnit :: { sRunUnit :: Sing u } -> SUnit (MkUnit u)

     Somewhat surprisingly, the type of sRunUnit:

       sRunUnit :: Sing (MkUnit u) -> Sing u

     Is not general enough to handle common uses of record selectors. For
     example, if you try to single this function:

       f :: Unit -> ()
       f = runUnit

     Then the resulting code:

       sF :: Sing (x :: Unit) -> Sing (F x :: ())
       sF = sRunUnit

     Will not typecheck. Note that sRunUnit expects an argument of type
     `Sing (MkUnit u)`, but there is no way to know a priori that the `x` in
     `Sing (x :: Unit)` is `MkUnit u` without pattern-matching on SMkUnit.

Hopefully I have convinced you that handling records in singletons-th is a bit of
a nightmare. Thankfully, there is a simple trick to avoid most of the pitfalls
above: just desugar code (using th-desugar) to avoid records!
In more concrete terms, we do the following:

* A record constructions desugars to a normal constructor application. For example:

    MkT{a = x, b = y}

      ==>

    MkT x y

  Something similar occurs for record syntax in patterns.

* A record update desugars to a case expression. For example:

    t{a = x}

      ==>

    case t of MkT _ y => MkT x y

We can't easily desugar away all uses of records, however. After all, records
can be used as ordinary functions as well. We leave such uses of records alone
when desugaring and accommodate them during promotion and singling by generating
"manual" record selectors. As a running example, consider the earlier Unit example:

  newtype Unit = MkUnit { runUnit :: () }

When singling Unit, we do not give SMkUnit a record selector:

  data SUnit :: Unit -> Type where
    SMkUnit :: Sing u -> SUnit (MkUnit u)

Instead, we generate a top-level function that behaves equivalently to runUnit.
This function then gets promoted and singled (in D.S.TH.Promote.promoteDecs and
D.S.TH.Single.singTopLevelDecs):

  type family RunUnit (x :: Unit) :: () where
    RunUnit (MkUnit x) = x

  sRunUnit :: Sing (x :: Unit) -> Sing (RunUnit x :: ())
  sRunUnit (SMkUnit sx) = sx

Now promoting/singling uses of runUnit as an ordinary function work as expected
since the types of RunUnit/sRunUnit are sufficiently general. This technique also
scales up to data types with multiple constructors sharing a record selector name.
For instance, in the earlier X example:

  data X = X1 {y :: Bool} | X2 {y :: Bool}

We would promote/single `y` like so:

  type family Y (x :: X) :: Bool where
    Y (X1 y) = y
    Y (X2 y) = y

  sY :: Sing (x :: X) -> Sing (Y x :: Bool)
  sY (SX1 sy) = sy
  sY (SX2 sy) = sy

Manual record selectors cannot be used in record constructions or updates, but
for most use cases this won't be an issue, since singletons-th makes an effort to
desugar away fancy uses of records anyway. The only time this would bite is if
you wanted to use record syntax in hand-written singletons code.
-}
