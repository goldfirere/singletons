{- Data/Singletons/Promote/Defun.hs

(c) Richard Eisenberg, Jan Stolarek 2014
rae@cs.brynmawr.edu

This file creates defunctionalization symbols for types during promotion.
-}

{-# LANGUAGE TemplateHaskell #-}

module Data.Singletons.Promote.Defun where

import Language.Haskell.TH.Desugar
import Data.Singletons.Promote.Monad
import Data.Singletons.Promote.Type
import Data.Singletons.Names
import Language.Haskell.TH.Syntax
import Data.Singletons.Syntax
import Data.Singletons.TH.Options
import Data.Singletons.Util
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe

defunInfo :: DInfo -> PrM [DDec]
defunInfo (DTyConI dec _instances) = buildDefunSyms dec
defunInfo (DPrimTyConI _name _numArgs _unlifted) =
  fail $ "Building defunctionalization symbols of primitive " ++
         "type constructors not supported"
defunInfo (DVarI _name _ty _mdec) =
  fail "Building defunctionalization symbols of values not supported"
defunInfo (DTyVarI _name _ty) =
  fail "Building defunctionalization symbols of type variables not supported"
defunInfo (DPatSynI {}) =
  fail "Building defunctionalization symbols of pattern synonyms not supported"

-- Defunctionalize type families defined at the top level (i.e., not associated
-- with a type class).
defunTopLevelTypeDecls ::
     [TySynDecl]
  -> [ClosedTypeFamilyDecl]
  -> [OpenTypeFamilyDecl]
  -> PrM ()
defunTopLevelTypeDecls ty_syns c_tyfams o_tyfams = do
  defun_ty_syns <-
    concatMapM (\(TySynDecl name tvbs rhs) -> buildDefunSymsTySynD name tvbs rhs) ty_syns
  defun_c_tyfams <-
    concatMapM (buildDefunSymsClosedTypeFamilyD . getTypeFamilyDecl) c_tyfams
  defun_o_tyfams <-
    concatMapM (buildDefunSymsOpenTypeFamilyD . getTypeFamilyDecl) o_tyfams
  emitDecs $ defun_ty_syns ++ defun_c_tyfams ++ defun_o_tyfams

-- Defunctionalize all the type families associated with a type class.
defunAssociatedTypeFamilies ::
     [DTyVarBndr]         -- The type variables bound by the parent class
  -> [OpenTypeFamilyDecl] -- The type families associated with the parent class
  -> PrM ()
defunAssociatedTypeFamilies cls_tvbs atfs = do
  defun_atfs <- concatMapM defun atfs
  emitDecs defun_atfs
  where
    defun :: OpenTypeFamilyDecl -> PrM [DDec]
    defun (TypeFamilyDecl tf_head) =
      buildDefunSymsTypeFamilyHead ascribe_tf_tvb_kind id tf_head

    -- Maps class-bound type variables to their kind annotations (if supplied).
    -- For example, `class C (a :: Bool) b (c :: Type)` will produce
    -- {a |-> Bool, c |-> Type}.
    cls_tvb_kind_map :: Map Name DKind
    cls_tvb_kind_map = Map.fromList [ (extractTvbName tvb, tvb_kind)
                                    | tvb <- cls_tvbs
                                    , Just tvb_kind <- [extractTvbKind tvb]
                                    ]

    -- If the parent class lacks a SAK, we cannot safely default kinds to
    -- Type. All we can do is make use of whatever kind information that parent
    -- class provides and let kind inference do the rest.
    --
    -- We can sometimes learn more specific information about unannotated type
    -- family binders from the parent class, as in the following example:
    --
    --   class C (a :: Bool) where
    --     type T a :: Type
    --
    -- Here, we know that `T :: Bool -> Type` because we can infer that the `a`
    -- in `type T a` should be of kind `Bool` from the class SAK.
    ascribe_tf_tvb_kind :: DTyVarBndr -> DTyVarBndr
    ascribe_tf_tvb_kind tvb =
      case tvb of
        DKindedTV{} -> tvb
        DPlainTV n  -> maybe tvb (DKindedTV n) $ Map.lookup n cls_tvb_kind_map

buildDefunSyms :: DDec -> PrM [DDec]
buildDefunSyms dec =
  case dec of
    DDataD _new_or_data _cxt _tyName _tvbs _k ctors _derivings ->
      buildDefunSymsDataD ctors
    DClosedTypeFamilyD tf_head _ ->
      buildDefunSymsClosedTypeFamilyD tf_head
    DOpenTypeFamilyD tf_head ->
      buildDefunSymsOpenTypeFamilyD tf_head
    DTySynD name tvbs rhs ->
      buildDefunSymsTySynD name tvbs rhs
    DClassD _cxt name tvbs _fundeps _members ->
      defunReify name tvbs (Just (DConT constraintName))
    _ -> fail $ "Defunctionalization symbols can only be built for " ++
                "type families and data declarations"

-- Unlike open type families, closed type families that lack SAKS do not
-- default anything to Type, instead relying on kind inference to figure out
-- unspecified kinds.
buildDefunSymsClosedTypeFamilyD :: DTypeFamilyHead -> PrM [DDec]
buildDefunSymsClosedTypeFamilyD = buildDefunSymsTypeFamilyHead id id

-- If an open type family lacks a SAK and has type variable binders or a result
-- without explicit kinds, then they default to Type (hence the uses of
-- default{Tvb,Maybe}ToTypeKind).
buildDefunSymsOpenTypeFamilyD :: DTypeFamilyHead -> PrM [DDec]
buildDefunSymsOpenTypeFamilyD =
  buildDefunSymsTypeFamilyHead defaultTvbToTypeKind (Just . defaultMaybeToTypeKind)

buildDefunSymsTypeFamilyHead
  :: (DTyVarBndr -> DTyVarBndr)   -- How to default each type variable binder
  -> (Maybe DKind -> Maybe DKind) -- How to default the result kind
  -> DTypeFamilyHead -> PrM [DDec]
buildDefunSymsTypeFamilyHead default_tvb default_kind
    (DTypeFamilyHead name tvbs result_sig _) = do
  let arg_tvbs = map default_tvb tvbs
      res_kind = default_kind (resultSigToMaybeKind result_sig)
  defunReify name arg_tvbs res_kind

buildDefunSymsTySynD :: Name -> [DTyVarBndr] -> DType -> PrM [DDec]
buildDefunSymsTySynD name tvbs rhs = defunReify name tvbs mb_res_kind
  where
    -- If a type synonym lacks a SAK, we can "infer" its result kind by
    -- checking for an explicit kind annotation on the right-hand side.
    mb_res_kind :: Maybe DKind
    mb_res_kind = case rhs of
                    DSigT _ k -> Just k
                    _         -> Nothing

buildDefunSymsDataD :: [DCon] -> PrM [DDec]
buildDefunSymsDataD ctors =
  concatMapM promoteCtor ctors
  where
    promoteCtor :: DCon -> PrM [DDec]
    promoteCtor (DCon tvbs _ name fields res_ty) = do
      let arg_tys = tysOfConFields fields
      arg_kis <- traverse promoteType_NC arg_tys
      res_ki  <- promoteType_NC res_ty
      let con_ki = ravelVanillaDType tvbs [] arg_kis res_ki
      m_fixity <- reifyFixityWithLocals name
      defunctionalize name m_fixity $ DefunSAK con_ki

-- Generate defunctionalization symbols for a name, using reifyFixityWithLocals
-- to determine what the fixity of each symbol should be
-- (see Note [Fixity declarations for defunctionalization symbols])
-- and dsReifyType to determine whether defunctionalization should make use
-- of SAKs or not (see Note [Defunctionalization game plan]).
defunReify :: Name           -- Name of the declaration to be defunctionalized
           -> [DTyVarBndr]   -- The declaration's type variable binders
                             -- (only used if the declaration lacks a SAK)
           -> Maybe DKind    -- The declaration's return kind, if it has one
                             -- (only used if the declaration lacks a SAK)
           -> PrM [DDec]
defunReify name tvbs m_res_kind = do
  m_fixity <- reifyFixityWithLocals name
  m_sak    <- dsReifyType name
  let defun = defunctionalize name m_fixity
  case m_sak of
    Just sak -> defun $ DefunSAK sak
    Nothing  -> defun $ DefunNoSAK tvbs m_res_kind

-- Generate symbol data types, Apply instances, and other declarations required
-- for defunctionalization.
-- See Note [Defunctionalization game plan] for an overview of the design
-- considerations involved.
defunctionalize :: Name
                -> Maybe Fixity
                -> DefunKindInfo
                -> PrM [DDec]
defunctionalize name m_fixity defun_ki = do
  case defun_ki of
    DefunSAK sak ->
      -- Even if a declaration has a SAK, its kind may not be vanilla.
      case unravelVanillaDType_either sak of
        -- If the kind isn't vanilla, use the fallback approach.
        -- See Note [Defunctionalization game plan],
        -- Wrinkle 2: Non-vanilla kinds.
        Left _ -> defun_fallback [] (Just sak)
        -- Otherwise, proceed with defun_vanilla_sak.
        Right (sak_tvbs, _sak_cxt, sak_arg_kis, sak_res_ki)
               -> defun_vanilla_sak sak_tvbs sak_arg_kis sak_res_ki
    -- If a declaration lacks a SAK, it likely has a partial kind.
    -- See Note [Defunctionalization game plan], Wrinkle 1: Partial kinds.
    DefunNoSAK tvbs m_res -> defun_fallback tvbs m_res
  where
    -- Generate defunctionalization symbols for things with vanilla SAKs.
    -- The symbols themselves will also be given SAKs.
    defun_vanilla_sak :: [DTyVarBndr] -> [DKind] -> DKind -> PrM [DDec]
    defun_vanilla_sak sak_tvbs sak_arg_kis sak_res_ki = do
      opts <- getOptions
      extra_name <- qNewName "arg"
      -- Use noExactName below to avoid #17537.
      arg_names <- replicateM (length sak_arg_kis) (noExactName <$> qNewName "a")

      let -- The inner loop. @go n arg_nks res_nks@ returns @(res_k, decls)@.
          -- Using one particular example:
          --
          -- @
          -- type ExampleSym2 :: a -> b -> c ~> d ~> Type
          -- data ExampleSym2 x y where ...
          -- type instance Apply (ExampleSym2 x y) z = ExampleSym3 x y z
          -- ...
          -- @
          --
          -- We have:
          --
          -- * @n@ is 2. This is incremented in each iteration of `go`.
          --
          -- * @arg_nks@ is [(x, a), (y, b)]. Each element in this list is a
          -- (type variable name, type variable kind) pair. The kinds appear in
          -- the SAK, separated by matchable arrows (->).
          --
          -- * @res_tvbs@ is [(z, c), (w, d)]. Each element in this list is a
          -- (type variable name, type variable kind) pair. The kinds appear in
          -- @res_k@, separated by unmatchable arrows (~>).
          --
          -- * @res_k@ is `c ~> d ~> Type`. @res_k@ is returned so that earlier
          --   defunctionalization symbols can build on the result kinds of
          --   later symbols. For instance, ExampleSym1 would get the result
          --   kind `b ~> c ~> d ~> Type` by prepending `b` to ExampleSym2's
          --   result kind `c ~> d ~> Type`.
          --
          -- * @decls@ are all of the declarations corresponding to ExampleSym2
          --   and later defunctionalization symbols. This is the main payload of
          --   the function.
          --
          -- This function is quadratic because it appends a variable at the end of
          -- the @arg_nks@ list at each iteration. In practice, this is unlikely
          -- to be a performance bottleneck since the number of arguments rarely
          -- gets to be that large.
          go :: Int -> [(Name, DKind)] -> [(Name, DKind)] -> (DKind, [DDec])
          go n arg_nks res_nkss =
            case res_nkss of
              [] ->
                let -- Somewhat surprisingly, we do *not* generate SAKs for
                    -- fully saturated defunctionalization symbols.
                    -- See Note [No SAKs for fully saturated defunctionalization symbols]
                    sat_decs = mk_sat_decs opts n (map (uncurry DKindedTV) arg_nks)
                                           (Just sak_res_ki)
                in (sak_res_ki, sat_decs)
              res_nk:res_nks ->
                let (res_ki, decs)   = go (n+1) (arg_nks ++ [res_nk]) res_nks
                    tyfun            = buildTyFunArrow (snd res_nk) res_ki
                    defun_sak_dec    = DKiSigD (defunctionalizedName opts name n) $
                                       ravelVanillaDType sak_tvbs [] (map snd arg_nks) tyfun
                    defun_other_decs = mk_defun_decs opts n (map (DPlainTV . fst) arg_nks)
                                                     (fst res_nk) extra_name Nothing
                in (tyfun, defun_sak_dec:defun_other_decs ++ decs)

      pure $ snd $ go 0 [] $ zip arg_names sak_arg_kis

    -- If defun_sak can't be used to defunctionalize something, this fallback
    -- approach is used. This is used when defunctionalizing something with a
    -- partial kind
    -- (see Note [Defunctionalization game plan], Wrinkle 1: Partial kinds)
    -- or a non-vanilla kind
    -- (see Note [Defunctionalization game plan], Wrinkle 2: Non-vanilla kinds).
    defun_fallback :: [DTyVarBndr] -> Maybe DKind -> PrM [DDec]
    defun_fallback tvbs' m_res' = do
      opts <- getOptions
      extra_name <- qNewName "arg"
      -- Use noExactTyVars below to avoid #11812.
      (tvbs, m_res) <- eta_expand (noExactTyVars tvbs') (noExactTyVars m_res')

      let -- The inner loop. @go n arg_tvbs res_tvbs@ returns @(m_res_k, decls)@.
          -- Using one particular example:
          --
          -- @
          -- data ExampleSym2 (x :: a) y :: c ~> d ~> Type where ...
          -- type instance Apply (ExampleSym2 x y) z = ExampleSym3 x y z
          -- ...
          -- @
          --
          -- This works very similarly to the `go` function in
          -- `defun_vanilla_sak`. The main differences are:
          --
          -- * This function does not produce any SAKs for defunctionalization
          --   symbols.
          --
          -- * Instead of [(Name, DKind)], this function uses [DTyVarBndr] as
          --   the types of @arg_tvbs@ and @res_tvbs@. This is because the
          --   kinds are not always known. By a similar token, this function
          --   uses Maybe DKind, not DKind, as the type of @m_res_k@, since
          --   the result kind is not always fully known.
          go :: Int -> [DTyVarBndr] -> [DTyVarBndr] -> (Maybe DKind, [DDec])
          go n arg_tvbs res_tvbss =
            case res_tvbss of
              [] ->
                let sat_decs = mk_sat_decs opts n arg_tvbs m_res
                in (m_res, sat_decs)
              res_tvb:res_tvbs ->
                let (m_res_ki, decs) = go (n+1) (arg_tvbs ++ [res_tvb]) res_tvbs
                    m_tyfun          = buildTyFunArrow_maybe (extractTvbKind res_tvb)
                                                             m_res_ki
                    defun_decs'      = mk_defun_decs opts n arg_tvbs
                                                     (extractTvbName res_tvb)
                                                     extra_name m_tyfun
                in (m_tyfun, defun_decs' ++ decs)

      pure $ snd $ go 0 [] tvbs

    mk_defun_decs :: Options
                  -> Int
                  -> [DTyVarBndr]
                  -> Name
                  -> Name
                  -> Maybe DKind
                  -> [DDec]
    mk_defun_decs opts n arg_tvbs tyfun_name extra_name m_tyfun =
      let data_name   = defunctionalizedName opts name n
          next_name   = defunctionalizedName opts name (n+1)
          con_name    = prefixName "" ":" $ suffixName "KindInference" "###" data_name
          arg_names   = map extractTvbName arg_tvbs
          params      = arg_tvbs ++ [DPlainTV tyfun_name]
          con_eq_ct   = DConT sameKindName `DAppT` lhs `DAppT` rhs
            where
              lhs = foldType (DConT data_name) (map DVarT arg_names) `apply` (DVarT extra_name)
              rhs = foldType (DConT next_name) (map DVarT (arg_names ++ [extra_name]))
          con_decl    = DCon [] [con_eq_ct] con_name (DNormalC False [])
                             (foldTypeTvbs (DConT data_name) params)
          data_decl   = DDataD Data [] data_name args m_tyfun [con_decl] []
            where
              args | isJust m_tyfun = arg_tvbs
                   | otherwise      = params
          app_data_ty = foldTypeTvbs (DConT data_name) arg_tvbs
          app_eqn     = DTySynEqn Nothing
                                  (DConT applyName `DAppT` app_data_ty
                                                   `DAppT` DVarT tyfun_name)
                                  (foldTypeTvbs (DConT next_name)
                                                (arg_tvbs ++ [DPlainTV tyfun_name]))
          app_decl    = DTySynInstD app_eqn
          suppress    = DInstanceD Nothing Nothing []
                          (DConT suppressClassName `DAppT` app_data_ty)
                          [DLetDec $ DFunD suppressMethodName
                                           [DClause []
                                                    ((DVarE 'snd) `DAppE`
                                                     mkTupleDExp [DConE con_name,
                                                                  mkTupleDExp []])]]

          -- See Note [Fixity declarations for defunctionalization symbols]
          fixity_decl = maybeToList $ fmap (mk_fix_decl data_name) m_fixity
      in data_decl : app_decl : suppress : fixity_decl

    -- Generate a "fully saturated" defunction symbol, along with a fixity
    -- declaration (if needed).
    mk_sat_decs :: Options -> Int -> [DTyVarBndr] -> Maybe DKind -> [DDec]
    mk_sat_decs opts n sat_tvbs m_sat_res =
      let sat_name = defunctionalizedName opts name n
          sat_dec  = DTySynD sat_name sat_tvbs $
                     foldTypeTvbs (DConT name) sat_tvbs `maybeSigT` m_sat_res
          sat_fixity_dec = maybeToList $ fmap (mk_fix_decl sat_name) m_fixity
      in sat_dec : sat_fixity_dec

    -- Generate extra kind variable binders corresponding to the number of
    -- arrows in the return kind (if provided). Examples:
    --
    -- >>> eta_expand [(x :: a), (y :: b)] (Just (c -> Type))
    -- ([(x :: a), (y :: b), (e :: c)], Just Type)
    --
    -- >>> eta_expand [(x :: a), (y :: b)] Nothing
    -- ([(x :: a), (y :: b)], Nothing)
    eta_expand :: [DTyVarBndr] -> Maybe DKind -> PrM ([DTyVarBndr], Maybe DKind)
    eta_expand m_arg_tvbs Nothing = pure (m_arg_tvbs, Nothing)
    eta_expand m_arg_tvbs (Just res_kind) = do
        let (arg_ks, result_k) = unravelDType res_kind
            vis_arg_ks = filterDVisFunArgs arg_ks
        extra_arg_tvbs <- traverse mk_extra_tvb vis_arg_ks
        pure (m_arg_tvbs ++ extra_arg_tvbs, Just result_k)

    -- Convert a DVisFunArg to a DTyVarBndr, generating a fresh type variable
    -- name if the DVisFunArg is an anonymous argument.
    mk_extra_tvb :: DVisFunArg -> PrM DTyVarBndr
    mk_extra_tvb vfa =
      case vfa of
        DVisFADep tvb -> pure tvb
        DVisFAAnon k  -> DKindedTV <$> qNewName "e" <*> pure k

    mk_fix_decl :: Name -> Fixity -> DDec
    mk_fix_decl n f = DLetDec $ DInfixD f n

-- Indicates whether the type being defunctionalized has a standalone kind
-- signature. If it does, DefunSAK contains the kind. If not, DefunNoSAK
-- contains whatever information is known about its type variable binders
-- and result kind.
-- See Note [Defunctionalization game plan] for details on how this
-- information is used.
data DefunKindInfo
  = DefunSAK DKind
  | DefunNoSAK [DTyVarBndr] (Maybe DKind)

-- Shorthand for building (k1 ~> k2)
buildTyFunArrow :: DKind -> DKind -> DKind
buildTyFunArrow k1 k2 = DConT tyFunArrowName `DAppT` k1 `DAppT` k2

buildTyFunArrow_maybe :: Maybe DKind -> Maybe DKind -> Maybe DKind
buildTyFunArrow_maybe m_k1 m_k2 = buildTyFunArrow <$> m_k1 <*> m_k2

{-
Note [Defunctionalization game plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generating defunctionalization symbols involves a surprising amount of
complexity. This Note gives a broad overview of what happens during
defunctionalization and highlights various design considerations.
As a working example, we will use the following type family:

  type Foo :: forall c a b. a -> b -> c -> c
  type family Foo x y z where ...

We must generate a defunctionalization symbol for every number of arguments
to which Foo can be partially applied. We do so by generating the following
declarations:

  type FooSym0 :: forall c a b. a ~> b ~> c ~> c
  data FooSym0 f where
   FooSym0KindInference :: SameKind (Apply FooSym0 arg) (FooSym1 arg)
                        => FooSym0 f
  type instance Apply FooSym0 x = FooSym1 x

  type FooSym1 :: forall c a b. a -> b ~> c ~> c
  data FooSym1 x f where
    FooSym1KindInference :: SameKind (Apply (FooSym1 a) arg) (FooSym2 a arg)
                         => FooSym1 a f
  type instance Apply (FooSym1 x) y = FooSym2 x y

  type FooSym2 :: forall c a b. a -> b -> c ~> c
  data FooSym2 x y f where
    FooSym2KindInference :: SameKind (Apply (FooSym2 x y) arg) (FooSym3 x y arg)
                         => FooSym2 x y f
  type instance Apply (FooSym2 x y) z = FooSym3 x y z

  type FooSym3 (x :: a) (y :: b) (z :: c) = Foo x y z :: c

Some things to note:

* Each defunctionalization symbol has its own standalone kind signature. The
  number after `Sym` in each symbol indicates the number of leading -> arrows
  in its kind—that is, the number of arguments to which it can be applied
  directly to without the use of the Apply type family.

  See "Wrinkle 1: Partial kinds" below for what happens if the declaration
  being defunctionalized does *not* have a standalone kind signature.

* Each data declaration has a constructor with the suffix `-KindInference`
  in its name. These are redundant in the particular case of Foo, where the
  kind is already known. They play a more vital role when the kind of the
  declaration being defunctionalized is only partially known.
  See "Wrinkle 1: Partial kinds" below for more information.

* FooSym3, the last defunctionalization symbol, is somewhat special in that
  it is a type synonym, not a data type. These sorts of symbols are referred
  to as "fully saturated" defunctionalization symbols. Furthermore, these
  symbols are intentionally *not* given SAKs. See
  Note [No SAKs for fully saturated defunctionalization symbols].

* If Foo had a fixity declaration (e.g., infixl 4 `Foo`), then we would also
  generate fixity declarations for each defunctionalization symbol (e.g.,
  infixl 4 `FooSym0`).
  See Note [Fixity declarations for defunctionalization symbols].

* Foo has a vanilla kind signature. (See
  Note [Vanilla-type validity checking during promotion] in D.S.Promote.Type
  for what "vanilla" means in this context.) Having a vanilla type signature is
  important, as it is a property that makes it much simpler to preserve the
  order of type variables (`forall c a b.`) in each of the defunctionalization
  symbols.

  That being said, it is not strictly required that the kind be vanilla. There
  is another approach that can be used to defunctionalize things with
  non-vanilla types, at the possible expense of having different type variable
  orders between different defunctionalization symbols.
  See "Wrinkle 2: Non-vanilla kinds" below for more information.

-----
-- Wrinkle 1: Partial kinds
-----

The Foo example above has a standalone kind signature, but not everything has
this much kind information. For example, consider this:

  $(singletons [d|
    type family Not x where
      Not False = True
      Not True  = False
    |])

The inferred kind for Not is `Bool -> Bool`, but since Not was declared in TH
quotes, `singletons` has no knowledge of this. Instead, we must rely on kind
inference to give Not's defunctionalization symbols the appropriate kinds.
Here is a naïve first attempt:

  data NotSym0 f
  type instance Apply NotSym0 x = NotSym1 x

  type NotSym1 x = Not x

NotSym1 will have the inferred kind `Bool -> Bool`, but poor NotSym0 will have
the inferred kind `forall k. k -> Type`, which is far more general than we
would like. We can do slightly better by supplying additional kind information
in a data constructor, like so:

  type SameKind :: k -> k -> Constraint
  class SameKind x y = ()

  data NotSym0 f where
    NotSym0KindInference :: SameKind (Apply NotSym0 arg) (NotSym1 arg)
                         => NotSym0 f

NotSym0KindInference is not intended to ever be seen by the user. Its only
reason for existing is its existential
`SameKind (Apply NotSym0 arg) (NotSym1 arg)` context, which allows GHC to
figure out that NotSym0 has kind `Bool ~> Bool`. This is a bit of a hack, but
it works quite nicely. The only problem is that GHC is likely to warn that
NotSym0KindInference is unused, which is annoying. To work around this, we
mention the data constructor in an instance of a dummy class:

  instance SuppressUnusedWarnings NotSym0 where
    suppressUnusedWarnings = snd (NotSym0KindInference, ())

Similarly, this SuppressUnusedWarnings class is not intended to ever be seen
by the user. As its name suggests, it only exists to help suppress "unused
data constructor" warnings.

Some declarations have a mixture of known kinds and unknown kinds, such as in
this example:

  $(singletons [d|
    type family Bar x (y :: Nat) (z :: Nat) :: Nat where ...
    |])

We can use the known kinds to guide kind inference. In this particular example
of Bar, here are the defunctionalization symbols that would be generated:

  data BarSym0 f where ...
  data BarSym1 x :: Nat ~> Nat ~> Nat where ...
  data BarSym2 x (y :: Nat) :: Nat ~> Nat where ...
  type BarSym3 x (y :: Nat) (z :: Nat) = Bar x y z :: Nat

-----
-- Wrinkle 2: Non-vanilla kinds
-----

There is only limited support for defunctionalizing declarations with
non-vanilla kinds. One example of something with a non-vanilla kind is the
following, which uses a nested forall:

  $(singletons [d|
    type Baz :: forall a. a -> forall b. b -> Type
    data Baz x y
    |])

One might envision generating the following defunctionalization symbols for
Baz:

  type BazSym0 :: forall a. a ~> forall b. b ~> Type
  data BazSym0 f where ...

  type BarSym1 :: forall a. a -> forall b. b ~> Type
  data BazSym1 x f where ...

  type family BazSym2 (x :: a) (y :: b) = Baz x y :: Type

Unfortunately, doing so would require impredicativity, since we would have:

    forall a. a ~> forall b. b ~> Type
  = forall a. (~>) a (forall b. b ~> Type)
  = forall a. TyFun a (forall b. b ~> Type) -> Type

Note that TyFun is an ordinary data type, so having its second argument be
(forall b. b ~> Type) is truly impredicative. As a result, trying to preserve
nested or higher-rank foralls is a non-starter.

We need not reject Baz entirely, however. We can still generate perfectly
usable defunctionalization symbols if we are willing to sacrifice the exact
order of foralls. When we encounter a non-vanilla kind such as Baz's, we simply
fall back to the algorithm used when we encounter a partial kind (as described
in "Wrinkle 1: Partial kinds" above.) In other words, we generate the
following symbols:

  data BazSym0 :: a ~> b ~> Type where ...
  data BazSym1 (x :: a) :: b ~> Type where ...
  type BazSym2 (x :: a) (y :: b) = Baz x y :: Type

The kinds of BazSym0 and BazSym1 both start with `forall a b.`,
whereas the `b` is quantified later in Baz itself. For most use cases, however,
this is not a huge concern.

Another way kinds can be non-vanilla is if they contain visible dependent
quantification, like so:

  $(singletons [d|
    type Quux :: forall (k :: Type) -> k -> Type
    data Quux x y
    |])

What should the kind of QuuxSym0 be? Intuitively, it should be this:

  type QuuxSym0 :: forall (k :: Type) ~> k ~> Type

Alas, `forall (k :: Type) ~>` simply doesn't work. See #304. But there is an
acceptable compromise we can make that can give us defunctionalization symbols
for Quux. Once again, we fall back to the partial kind algorithm:

  data QuuxSym0 :: Type ~> k ~> Type where ...
  data QuuxSym1 (k :: Type) :: k ~> Type where ...
  type QuuxSym2 (k :: Type) (x :: k) = Quux k x :: Type

The catch is that the kind of QuuxSym0, `forall k. Type ~> k ~> Type`, is
slightly more general than it ought to be. In practice, however, this is
unlikely to be a problem as long as you apply QuuxSym0 to arguments of the
right kinds.

Note [No SAKs for fully saturated defunctionalization symbols]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When generating defunctionalization symbols, most of the symbols are data
types. The last one, however, is a type synonym. For example, this code:

  $(singletons [d|
    type Const :: a -> b -> a
    type Const x y = x
    |])

Will generate the following symbols:

  type ConstSym0 :: a ~> b ~> a
  data ConstSym0 f where ...

  type ConstSym1 :: a -> b ~> a
  data ConstSym1 x f where ...

  type ConstSym2 (x :: a) (y :: b) = Const x y :: a

ConstSym2, the sole type synonym of the bunch, is what is referred to as a
"fully saturated" defunctionaliztion symbol.

At first glance, ConstSym2 may not seem terribly useful, since it is
effectively a thin wrapper around the original Const type. Indeed, fully
saturated symbols are never appear directly in user-written code. Instead,
they are most valuable in TH-generated code, as singletons often generates code
that directly applies a defunctionalization symbol to some number of arguments
(see, for instance, D.S.Names.promoteTySym). In theory, such code could carve
out a special case for fully saturated applications and apply the original
type instead of a defunctionalization symbol, but determining when an
application is fully saturated is often difficult in practice. As a result, it
is more convenient to just generate code that always applies FuncSymN to N
arguments, and to let fully saturated defunctionalization symbols handle the
case where N equals the number of arguments needed to fully saturate Func.

Another curious thing about fully saturated defunctionalization symbols do
*not* get assigned SAKs, unlike their data type brethren. Why not just give
ConstSym2 a SAK like this?

  type ConstSym2 :: a -> b -> a
  type ConstSym2 x y = Const x y

This would in fact work for most use cases, but there are a handful of corner
cases where this approach would break down. Here is one such corner case:

  $(promote [d|
    class Applicative f where
      pure :: a -> f a
      ...
      (*>) :: f a -> f b -> f b
    |])

  ==>

  class PApplicative f where
    type Pure (x :: a) :: f a
    type (*>) (x :: f a) (y :: f b) :: f b

What would happen if we were to defunctionalize the promoted version of (*>)?
We'd end up with the following defunctionalization symbols:

  type (*>@#@$)   :: f a ~> f b ~> f b
  data (*>@#@$) f where ...

  type (*>@#@$$)  :: f a -> f b ~> f b
  data (*>@#@$$) x f where ...

  type (*>@#@$$$) :: f a -> f b -> f b
  type (*>@#@$$$) x y = (*>) x y

It turns out, however, that (*>@#@$$$) will not kind-check. Because (*>@#@$$$)
has a standalone kind signature, it is kind-generalized *before* kind-checking
the actual definition itself. Therefore, the full kind is:

  type (*>@#@$$$) :: forall {k} (f :: k -> Type) (a :: k) (b :: k).
                     f a -> f b -> f b
  type (*>@#@$$$) x y = (*>) x y

However, the kind of (*>) is
`forall (f :: Type -> Type) (a :: Type) (b :: Type). f a -> f b -> f b`.
This is not general enough for (*>@#@$$$), which expects kind-polymorphic `f`,
`a`, and `b`, leading to a kind error. You might think that we could somehow
infer this information, but note the quoted definition of Applicative (and
PApplicative, as a consequence) omits the kinds of `f`, `a`, and `b` entirely.
Unless we were to implement full-blown kind inference inside of Template
Haskell (which is a tall order), the kind `f a -> f b -> f b` is about as good
as we can get.

Note that (*>@#@$) and (*>@#@$$) are implemented as GADTs, not type synonyms.
This allows them to have kind-polymorphic `f`, `a`, and `b` in their kinds
while equating `k` to be `Type` in their data constructors, which neatly avoids
the issue that (*>@#@$$$) faces.

-----

In one last attempt to salvage the idea of giving SAKs to fully saturated
defunctionalization symbols, I explored an idea where we would add
"dummy constraints" to get the kinds exactly right. The idea was to first
define a type synonym for dummy contexts:

  type Dummy :: Constraint -> Constraint
  type Dummy x = () ~ ()

Dummy simply ignores its argument and returns `() ~ ()`. `() ~ ()` was chosen
because it's one of the few Constraints that can currently be used at the kind
level. Dummy could, in theory, be used like this:

  type (*>@#@$)   :: Dummy (PApplicative f) => f a ~> f b ~> f b
  type (*>@#@$$)  :: Dummy (PApplicative f) => f a -> f b ~> f b
  type (*>@#@$$$) :: Dummy (PApplicative f) => f a -> f b -> f b

The advantage to using `Dummy (PApplicative f)` is that it would constraint `f`
to be of kind `Type -> Type`, which would get the kinds exactly the way we want
them. Sounds great, right? Unfortunately, it doesn't work in practice. Consider
this example:

  $(promoteOnly [d|
    class C a where
      m1 :: a -> a
      m1 = m2

      m2 :: a -> a
      m2 = m1
    |])

  ==>

  class PC a where
    type M1 (x :: a) :: a
    type M1 x = Apply M2Sym1 x

    type M2 (x :: a) :: a
    type M2 x = Apply M1Sym1 x

The generated code would fail to compile, instead throwing this error:

  error:
      • Class ‘PC’ cannot be used here
          (it is defined and used in the same recursive group)
      • In the first argument of ‘Dummy’, namely ‘(PC a)’
        In a standalone kind signature for ‘M2Sym1’:
          forall a. Dummy (PC a) => a -> a
     |
     | type M2Sym1 :: forall a. Dummy (PC a) => a -> a
     |                                 ^^^^

Ugh. I suspect this is a GHC bug (see
https://gitlab.haskell.org/ghc/ghc/issues/15942#note_242075), but it's one
that's unlikely to be fixed any time soon.

A slight variations on idea is to use the original class instead of the
promoted class in `Dummy` contexts, e.g.,

  type M2Sym1 :: forall a. Dummy (C a) => a -> a

This would avoid the recursive group issues, but it would introduce a new
problem: the original class is not guaranteed to exist if
`promoteOnly` or `singletonsOnly` are used to create the promoted class.
(Indeed, this is precisely the case in the `PC` example.)

-----

As an alternative to type synonyms, we might consider using type families to
define fully saturated defunctionalization symbols. For instance, we could try
this:

  type (*>@#@$$$) :: f a -> f b -> f b
  type family (*>@#@$$$) x y where
    (*>@#@$$$) x y = (*>) x y

Like before, the full kind of (*>@#@$$$) is generalized to be
`forall {k} (f :: k -> Type) (a :: k) (b :: k)`. The difference is that the
type family equation *matches* on `k` such that the equation will only trigger
if `k` is equal to `Type`. (This is similar to the trick that (*>@#@$) and
(*>@#@$$) employ, as being GADTs allows them to constrain `k` to be `Type` in
their data constructors.)

Alas, the type family approach is strictly less powerful than the type synonym
approach. Consider the following code:

  $(singletons [d|
    data Nat = Z | S Nat

    natMinus :: Nat -> Nat -> Nat
    natMinus Z     _     = Z
    natMinus (S a) (S b) = natMinus a b
    natMinus a     Z     = a
    |])

Among other things, this will generate the following declarations:

  type ZSym0 :: Nat

  type NatMinus :: Nat -> Nat -> Nat
  type family NatMinus x y where
    NatMinus Z     _     = ZSym0
    NatMinus (S a) (S b) = NatMinus a b
    NatMinus a     Z     = a

  sNatMinus :: SNat x -> SNat y -> SNat (NatMinus x y)
  sNatMinus SZ      _       = SZ
  sNatMinus (SS sA) (SS sB) = sNatMinus sA sB
  sNatMinus sA      SZ      = sA

Shockingly, this will either succeed or fail to compile depending on whether
ZSym0 is a type synonym or a type family. If ZSym0 is a type synonym, then
the first and third equations of NatMinus will be compatible (since GHC will
be able to infer that Z ~ ZSym0), which is what allows the third equation of
sNatMinus to typecheck. If ZSym0 is a type family, however, then the third
equation of NatMinus will be incompatible with the first, which will cause
the third equation of sNatMinus to fail to typecheck:

  error:
      • Could not deduce: NatMinus x 'Z ~ x
        from the context: y ~ 'Z
          bound by a pattern with constructor: SZ :: SNat 'Z,
                   in an equation for ‘sNatMinus’
        ‘x’ is a rigid type variable bound by
          the type signature for:
            sNatMinus :: forall (x :: Nat) (y :: Nat).
                         SNat x -> SNat y -> SNat (NatMinus x y)
        Expected type: SNat (NatMinus x y)
          Actual type: SNat x
      • In the expression: sA
        In an equation for ‘sNatMinus’: sNatMinus sA SZ = sA
      • Relevant bindings include
          sA :: SNat x
          sNatMinus :: SNat x -> SNat y -> SNat (NatMinus x y)

One could work around the issue by tweaking the third equation of natMinus
slightly:

  $(singletons [d|
    ...

    natMinus :: Nat -> Nat -> Nat
    natMinus Z       _     = Z
    natMinus (S a)   (S b) = natMinus a b
    natMinus a@(S _) Z     = a
    |])

But I would generally prefer to avoid having the user add extraneous pattern
matches when possible. Given the choice between expressiveness and SAKs, I give
the edge to expressiveness.

Bottom line: don't give fully saturated defunctionalization symbols SAKs. This
is admittedly not ideal, but it's unlikely to be a sticking point in practice,
given that these symbols are almost exclusively used in autogenerated code
in the first place. If we want to support promoting code that uses visible
type application (see #378), we will need to figure out how to resolve this
issue.

Note [Fixity declarations for defunctionalization symbols]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Just like we promote fixity declarations, we should also generate fixity
declarations for defunctionaliztion symbols. A primary use case is the
following scenario:

  (.) :: (b -> c) -> (a -> b) -> (a -> c)
  (f . g) x = f (g x)
  infixr 9 .

One often writes (f . g . h) at the value level, but because (.) is promoted
to a type family with three arguments, this doesn't directly translate to the
type level. Instead, one must write this:

  f .@#@$$$ g .@#@$$$ h

But in order to ensure that this associates to the right as expected, one must
generate an `infixr 9 .@#@#$$$` declaration. This is why defunctionalize accepts
a Maybe Fixity argument.
-}
