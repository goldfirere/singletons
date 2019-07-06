{- Data/Singletons/Promote/Defun.hs

(c) Richard Eisenberg, Jan Stolarek 2014
rae@cs.brynmawr.edu

This file creates defunctionalization symbols for types during promotion.
-}

{-# LANGUAGE TemplateHaskell #-}

module Data.Singletons.Promote.Defun where

import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OSet as OSet
import Data.Singletons.Promote.Monad
import Data.Singletons.Promote.Type
import Data.Singletons.Names
import Language.Haskell.TH.Syntax
import Data.Singletons.Syntax
import Data.Singletons.Util
import Control.Monad
import Data.Foldable
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

defunTypeDecls :: [TySynDecl]
               -> [ClosedTypeFamilyDecl]
               -> [OpenTypeFamilyDecl]
               -> PrM ()
defunTypeDecls ty_syns c_tyfams o_tyfams = do
  defun_ty_syns <-
    concatMapM (\(TySynDecl name tvbs) -> buildDefunSymsTySynD name tvbs) ty_syns
  defun_c_tyfams <-
    concatMapM (buildDefunSymsClosedTypeFamilyD . getTypeFamilyDecl) c_tyfams
  defun_o_tyfams <-
    concatMapM (buildDefunSymsOpenTypeFamilyD . getTypeFamilyDecl) o_tyfams
  emitDecs $ defun_ty_syns ++ defun_c_tyfams ++ defun_o_tyfams

buildDefunSyms :: DDec -> PrM [DDec]
buildDefunSyms (DDataD _new_or_data _cxt _tyName _tvbs _k ctors _derivings) =
  buildDefunSymsDataD ctors
buildDefunSyms (DClosedTypeFamilyD tf_head _) =
  buildDefunSymsClosedTypeFamilyD tf_head
buildDefunSyms (DOpenTypeFamilyD tf_head) =
  buildDefunSymsOpenTypeFamilyD tf_head
buildDefunSyms (DTySynD name tvbs _type) =
  buildDefunSymsTySynD name tvbs
buildDefunSyms (DClassD _cxt name tvbs _fundeps _members) = do
  defunReifyFixity name tvbs (Just (DConT constraintName))
buildDefunSyms _ = fail $ "Defunctionalization symbols can only be built for " ++
                          "type families and data declarations"

buildDefunSymsClosedTypeFamilyD :: DTypeFamilyHead -> PrM [DDec]
buildDefunSymsClosedTypeFamilyD = buildDefunSymsTypeFamilyHead id id

buildDefunSymsOpenTypeFamilyD :: DTypeFamilyHead -> PrM [DDec]
buildDefunSymsOpenTypeFamilyD = buildDefunSymsTypeFamilyHead cuskify default_to_star
  where
    default_to_star :: Maybe DKind -> Maybe DKind
    default_to_star Nothing  = Just $ DConT typeKindName
    default_to_star (Just k) = Just k

buildDefunSymsTypeFamilyHead
  :: (DTyVarBndr -> DTyVarBndr)
  -> (Maybe DKind -> Maybe DKind)
  -> DTypeFamilyHead -> PrM [DDec]
buildDefunSymsTypeFamilyHead default_tvb default_kind
    (DTypeFamilyHead name tvbs result_sig _) = do
  let arg_tvbs = map default_tvb tvbs
      res_kind = default_kind (resultSigToMaybeKind result_sig)
  defunReifyFixity name arg_tvbs res_kind

buildDefunSymsTySynD :: Name -> [DTyVarBndr] -> PrM [DDec]
buildDefunSymsTySynD name tvbs =
  defunReifyFixity name tvbs Nothing

buildDefunSymsDataD :: [DCon] -> PrM [DDec]
buildDefunSymsDataD ctors =
  concatMapM promoteCtor ctors
  where
    promoteCtor :: DCon -> PrM [DDec]
    promoteCtor ctor@(DCon _ _ _ _ res_ty) = do
      let (name, arg_tys) = extractNameTypes ctor
      tvb_names <- replicateM (length arg_tys) $ qNewName "t"
      arg_kis <- mapM promoteType arg_tys
      let arg_tvbs = zipWith DKindedTV tvb_names arg_kis
      res_ki <- promoteType res_ty
      defunReifyFixity name arg_tvbs (Just res_ki)

-- Generate defunctionalization symbols for a name, using reifyFixityWithLocals
-- to determine what the fixity of each symbol should be.
-- See Note [Fixity declarations for defunctionalization symbols]
defunReifyFixity :: Name -> [DTyVarBndr] -> Maybe DKind -> PrM [DDec]
defunReifyFixity name tvbs m_res_kind = do
  m_fixity <- reifyFixityWithLocals name
  defunctionalize name m_fixity tvbs m_res_kind

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
-- data FooSym2 a b f where
--   FooSym2KindInference :: SameKind (Apply (FooSym2 a b) arg) (FooSym3 a b arg)
--                        => FooSym2 a b f
-- type instance Apply (FooSym2 a b) c = FooSym3 a b c
-- data FooSym1 a f where
--   FooSym1KindInference :: SameKind (Apply (FooSym1 a) arg) (FooSym2 a arg)
--                        => FooSym1 a f
-- type instance Apply (FooSym1 a) b = FooSym2 a b
-- data FooSym0 f where
--  FooSym0KindInference :: SameKind (Apply FooSym0 arg) (FooSym1 arg)
--                       => FooSym0 f
-- type instance Apply FooSym0 a = FooSym1 a
--
-- What's up with all the "KindInference" stuff? In some scenarios, we don't
-- know the kinds that we should be using in these symbols. But, GHC can figure
-- it out using the types of the "KindInference" dummy data constructors. A
-- bit of a hack, but it works quite nicely. The only problem is that GHC will
-- warn about an unused data constructor. So, we use the data constructor in
-- an instance of a dummy class. (See Data.Singletons.SuppressUnusedWarnings
-- for the class, which should never be seen by anyone, ever.)
--
-- The defunctionalize function takes Maybe DKinds so that the caller can
-- indicate which kinds are known and which need to be inferred.
--
-- See also Note [Defunctionalization and dependent quantification]
defunctionalize :: Name
                -> Maybe Fixity -- The name's fixity, if one was declared.
                -> [DTyVarBndr] -> Maybe DKind -> PrM [DDec]
defunctionalize name m_fixity m_arg_tvbs' m_res_kind' = do
  (m_arg_tvbs, m_res_kind) <- eta_expand (noExactTyVars m_arg_tvbs')
                                         (noExactTyVars m_res_kind')

  let -- Implements part (2)(i) from Note [Defunctionalization and dependent quantification]
      tvb_to_type_map :: Map Name DType
      tvb_to_type_map = Map.fromList $                   -- (2)(i)(c)
                        map (\tvb -> (extractTvbName tvb, dTyVarBndrToDType tvb)) $
                        toposortTyVarsOf $               -- (2)(i)(b)
                        map dTyVarBndrToDType m_arg_tvbs
                          ++ maybeToList m_res_kind      -- (2)(i)(a)

      go :: Int -> [DTyVarBndr] -> Maybe DKind
         -> ([DTyVarBndr] -> DType)  -- given the argument tyvar binders,
                                     -- produce the RHS of the Apply instance
         -> PrM [DDec]
      go _ [] _ _ = return []
      go n (m_arg : m_args) m_result mk_rhs = do
        extra_name <- qNewName "arg"
        let tyfun_name  = extractTvbName m_arg
            data_name   = promoteTySym name n
            next_name   = promoteTySym name (n+1)
            con_name    = prefixName "" ":" $ suffixName "KindInference" "###" data_name
            m_tyfun     = buildTyFunArrow_maybe (extractTvbKind m_arg) m_result
            arg_params  = -- Implements part (2)(ii) from
                          -- Note [Defunctionalization and dependent quantification]
                          map (map_tvb_kind (substType tvb_to_type_map)) $
                          reverse m_args
            arg_names   = map extractTvbName arg_params
            params      = arg_params ++ [DPlainTV tyfun_name]
            con_eq_ct   = DConT sameKindName `DAppT` lhs `DAppT` rhs
              where
                lhs = foldType (DConT data_name) (map DVarT arg_names) `apply` (DVarT extra_name)
                rhs = foldType (DConT next_name) (map DVarT (arg_names ++ [extra_name]))
            con_decl    = DCon (map dropTvbKind params ++ [DPlainTV extra_name])
                               [con_eq_ct]
                               con_name
                               (DNormalC False [])
                               (foldTypeTvbs (DConT data_name) params)
            data_decl   = DDataD Data [] data_name args res_ki [con_decl] []
              where
                (args, res_ki)
                  = case m_tyfun of
                      Nothing    -> (params, Nothing)
                                    -- If we cannot infer the return type, don't bother
                                    -- trying to construct an explicit return kind.
                      Just tyfun ->
                        let bound_tvs = OSet.fromList (map extractTvbName arg_params)
                                        `OSet.union` foldMap (foldMap fvDType)
                                                             (map extractTvbKind arg_params)
                            not_bound tvb = not (extractTvbName tvb `OSet.member` bound_tvs)
                            tvb_to_type tvb_name = fromMaybe (DVarT tvb_name) $
                                                   Map.lookup tvb_name tvb_to_type_map
                            -- Implements part (2)(iii) from
                            -- Note [Defunctionalization and dependent quantification]
                            tyfun_tvbs = filter not_bound $     -- (2)(iii)(d)
                                         toposortTyVarsOf $     -- (2)(iii)(c)
                                         map tvb_to_type $      -- (2)(iii)(b)
                                         toList $ fvDType tyfun -- (2)(iii)(a)
                        in (arg_params, Just (DForallT tyfun_tvbs [] tyfun))
            app_data_ty = foldTypeTvbs (DConT data_name) m_args
            app_eqn     = DTySynEqn Nothing
                                    (DConT applyName `DAppT` app_data_ty
                                                     `DAppT` DVarT tyfun_name)
                                    (mk_rhs (m_args ++ [DPlainTV tyfun_name]))
            app_decl    = DTySynInstD app_eqn
            suppress    = DInstanceD Nothing Nothing []
                            (DConT suppressClassName `DAppT` app_data_ty)
                            [DLetDec $ DFunD suppressMethodName
                                             [DClause []
                                                      ((DVarE 'snd) `DAppE`
                                                       mkTupleDExp [DConE con_name,
                                                                    mkTupleDExp []])]]

            mk_rhs'     = foldTypeTvbs (DConT data_name)

            -- See Note [Fixity declarations for defunctionalization symbols]
            mk_fix_decl f = DLetDec $ DInfixD f data_name
            fixity_decl   = maybeToList $ fmap mk_fix_decl m_fixity

        decls <- go (n - 1) m_args m_tyfun mk_rhs'
        return $ suppress : data_decl : app_decl : fixity_decl ++ decls

  let num_args = length m_arg_tvbs
      sat_name = promoteTySym name num_args
      mk_rhs   = foldTypeTvbs (DConT name)
      sat_dec  = DTySynD sat_name m_arg_tvbs (mk_rhs m_arg_tvbs)

  other_decs <- go (num_args - 1) (reverse m_arg_tvbs) m_res_kind mk_rhs
  return $ sat_dec : other_decs
  where
    eta_expand :: [DTyVarBndr] -> Maybe DKind -> PrM ([DTyVarBndr], Maybe DKind)
    eta_expand m_arg_tvbs Nothing = pure (m_arg_tvbs, Nothing)
    eta_expand m_arg_tvbs (Just res_kind) = do
        let (_, _, argKs, resultK) = unravel res_kind
        tvb_names <- replicateM (length argKs) $ qNewName "e"
        let res_kind_arg_tvbs = zipWith DKindedTV tvb_names argKs
        pure (m_arg_tvbs ++ res_kind_arg_tvbs, Just resultK)

    map_tvb_kind :: (DKind -> DKind) -> DTyVarBndr -> DTyVarBndr
    map_tvb_kind _ tvb@DPlainTV{}  = tvb
    map_tvb_kind f (DKindedTV n k) = DKindedTV n (f k)

{-
Note [Defunctionalization and dependent quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The machinery in this module supports defunctionalizing types that use
dependent quantification, such as in the following example:

  type family Symmetry (a :: Proxy t) (y :: Proxy t)
                       (e :: (a :: Proxy (t :: k)) :~: (y :: Proxy (t :: k))) :: Type where
    Symmetry a y _ = y :~: a

Here is what is involved in making this happen:

1. When defunctionalizing, we must not only know the argument kinds, but rather
   the argument *kind variable binders*. This is essential since, for instance,
   Symmetry dependently quantifies `a` and `y` and uses them in the kind of
   `e`. If we did not track the original kind variable names, then instead of
   generating this defunctionalization symbol for Symmetry:

     data SymmetrySym2 (a :: Proxy t) (y :: Proxy t) :: (a :~: y) ~> Type

   We would generate something more general, like this:

     data SymmetrySym2 (abc1 :: Proxy t) (abc2 :: Proxy t) :: (a :~: y) ~> Type

   Alas, there are times where will have no choice but to write a slightly
   more general kind than we should. For instance, consider this:

     data SymmetrySym0 :: Proxy t ~> Proxy t ~> (a :~: y) ~> Type

   This defunctionalization symbol doesn't capture the dependent quantification
   in the first and second argument kinds. But in order to do that properly,
   you'd need the ability to write something like:

     data SymmetrySym0 :: forall (a :: Proxy t) ~> forall (y :: Proxy t)
                       ~> (a :~: y) ~> Type

   It is my (RGS's) belief that it is not possible to achieve something like
   this in today's GHC (see #304), so we'll just have to live with SymmetrySym0
   being slightly more general than it ought to be. In practice, this is
   unlikely to bite unless you're writing code that specifically exploits this
   dependency in just the right way.

2. I pulled a fast one earlier by writing:

     data SymmetrySym0 :: Proxy t ~> Proxy t ~> (a :~: y) ~> Type

   GHC will actually reject this, because it does not have a CUSK. There are
   two glaring problems here:

   (a) The kind of `t` is underdetermined.
   (b) `a` and `y` should have kind `Proxy t`, but this is not currently the case.

   Ultimately, the fix is to use explicit kind signatures. A naïve attempt
   would be something like this:

     data SymmetrySym0 :: Proxy (t :: (k :: Type)) ~> Proxy (t :: (k :: Type))
                       ~> ((a :: Proxy (t :: (k :: Type))) :~: (y :: Proxy (t :: (k :: Type))))
                       ~> Type

   While that works, it adds a nontrivial amount of clutter. Plus, it requires
   figuring out (in Template Haskell) which variables have underdetermined
   kinds and substituting for them. Blegh. A much cleaner approach is:

     data SymmetrySym0 :: forall (k :: Type) (t :: k) (a :: Proxy t) (y :: Proxy t).
                          Proxy t ~> Proxy t ~> (a :~: y) ~> Type

   This time, all we have to do is put an explicit `forall` in front, and we
   achieve a CUSK without having to muck up the body of return kind. It also
   has the benefit of looking much nicer in generated code.

   Let's talk about how to achieve this feat, using SymmetrySym1 as the
   guiding example:

   (i) Before we begin defunctionalizing a type, we construct a mapping from
       variable names to their corresponding types, complete with kinds.
       For instance, in Symmetry, we would have the following map:

         { k :-> DVarT k                                         -- k
         , t :-> DSigT (DVarT t) (DVarT k)                       -- (t :: k)
         , a :-> DSigT (DVarT a) (DConT ''Proxy `DAppT` DVarT t) -- (a :: Proxy t)
         , y :-> DSigT (DVarT y) (DConT ''Proxy `DAppT` DVarT y) -- (y :: Proxy t)
         , e :-> DSigT (DVarT e) (DConT ''(:~:)
                                  `DAppT` DSigT (DVarT a) (DConT ''Proxy `DAppT` DSigT (DVarT t) (DVarT k))
                                  `DAppT` DSigT (DVarT y) (DConT ''Proxy `DAppT` DSigT (DVarT t) (DVarT k)))
                                                                 -- (e :: (a :: Proxy (t :: k)) :~: (y :: Proxy (t :: k)))
         }

       Why do this? Because when constructing the `forall` in the return kind
       of a defunctionalization symbol, it's convenient to be able to know
       the kinds of everything being bound at a glance. It's not always
       possible to recover the kinds of every variable (for instance, if
       we're just given `Proxy t ~> Proxy t ~> (a :~: y) ~> Type`), so having
       this information is handy.

       To construct this map, we:

       (a) Grab the list of type variable binders (this is given as an input
           to defunctionalize, as discussed in part (1)) and turn it into a list
           of types. Also include the return kind (if there is one) in this
           list, as it may also mention type variables with explicit kinds.
       (b) Construct a flat list of all type variables mentioned in this list.
           This may involve looking in the kinds of type variables binders.
           (Note that this part is crucial—the the Singletons/PolyKinds test
           will fail to compile without it!)
       (c) Take the flat list and insert each variable into the map by
           mapping its name to its type (as demonstrated above).

       To continue the Symmetry example:

       (a) We grab the list of type variable binders

             [ (a :: Proxy t)
             , (y :: Proxy t)
             , (e :: (a :: Proxy (t :: k)) :~: (y :: Proxy (t :: k)))
             ]

           from the Symmetry declaration. Including the return kind (Type),
           we get:

             [ (a :: Proxy t)
             , (y :: Proxy t)
             , (e :: (a :: Proxy (t :: k)) :~: (y :: Proxy (t :: k)))
             , Type
             ]

       (b) We flatten this into a list of well scoped type variables:

             [ k
             , (t :: k)
             , (a :: Proxy t)
             , (y :: Proxy t)
             , (e :: (a :: Proxy (t :: k)) :~: (y :~: Proxy (t :: k)))
             ]

       (c) From this, we construct the map shown at the beginning of (i).

   (ii) Using the map, we will annotate any kind variables in the LHS of the
        declaration with their respective kinds. In this example, the LHS is:

          data SymmetrySym1 (a :: Proxy t) :: ...

        Since `t` maps to simply `(t :: k)` in the map, the LHS becomes:

          data SymmetrySym1 (a :: Proxy (t :: k)) :: ...

        Why do this? Because we need to make it apparent that `k` is bound on
        the LHS. If we don't, we might end up trying to quantify `k` in the
        return kind (see #353 for an example of what goes wrong if you try to
        do this).

        Having to explicitly annotate each occurrence of every kind variable on
        the LHS like this is a bit tiresome, especially since we don't have to
        do this in the return kind. If GHC had syntax for visible dependent
        quantification, we could avoid this step entirely and simply write:

          data SymmetrySym1 :: forall k (t :: k). forall (a :: Proxy t) -> ...

        Until GHC gains this syntax, this is the best alternative.

   (iii) When constructing each defunctionalization symbol, we will end up with
         some remaining type variable binders and a return kind. For instance:

           data SymmetrySym1 (a :: Proxy (t :: k))
             :: forall ???. Proxy t
                         ~> ((a :: Proxy (t :: k)) :~: (y :: Proxy (t :: k)))
                         ~> Type

         We must fill in the ??? part. Here is how we do so:

         (a) Collect all of the type variables mentioned in the return kind.
         (b) Look up each type variable's corresponding type in the map (from
             part (i)) to learn as much kind information as possible.
         (c) Perform a reverse topological sort on these types to put the
             types (and kind) variables in proper dependency order.
         (d) Filter out any variables that are already bound by the type
             variable binders that precede the return kind.

         After doing these steps, what remains goes in place of ???. Let's
         explain this with the example above:

           data SymmetrySym1 (a :: Proxy (t :: k))
             :: forall ???. Proxy t
                         ~> ((a :: Proxy (t :: k)) :~: (y :: Proxy (t :: k)))
                         ~> Type

         (a) [t, a, k, y]
         (b) [(t :: k), (a :: Proxy t), k, (y :: Proxy t)]
         (c) [k, (t :: k), (a :: Proxy t), (y :: Proxy t)]
         (d) [(y :: Proxy t)] (`k`, `t` and `a` were already bound)

         Therefore, we end up with:

           data SymmetrySym1 (a :: Proxy (t :: k))
             :: forall (y :: Proxy t).
                            Proxy t
                         ~> ((a :: Proxy (t :: k)) :~: (y :: Proxy (t :: k)))
                         ~> Type
-}

-- This is a small function with large importance. When generating
-- defunctionalization data types, we often need to fill in the blank in the
-- sort of code exemplified below:
--
-- @
-- data FooSym2 a (b :: x) (c :: TyFun y z) where
--   FooSym2KindInference :: _
-- @
--
-- Where the kind of @a@ is not known. It's extremely tempting to just
-- copy-and-paste the type variable binders from the data type itself to the
-- constructor, like so:
--
-- @
-- data FooSym2 a (b :: x) (c :: TyFun y z) where
--   FooSym2KindInference :: forall a (b :: x) (c :: TyFun y z).
--                           SameKind (...) (...).
--                           FooSym2KindInference a b c
-- @
--
-- But this ends up being an untenable approach. Because @a@ lacks a kind
-- signature, @FooSym2@ does not have a complete, user-specified kind signature
-- (or CUSK), so GHC will fail to typecheck @FooSym2KindInference@.
--
-- Thankfully, there's a workaround—just don't give any of the constructor's
-- type variable binders any kinds:
--
-- @
-- data FooSym2 a (b :: x) (c :: TyFun y z) where
--   FooSym2KindInference :: forall a b c
--                           SameKind (...) (...).
--                           FooSym2KindInference a b c
-- @
--
-- GHC may be moody when it comes to CUSKs, but it's at least understanding
-- enough to typecheck this without issue. The 'dropTvbKind' function is
-- what removes the kinds used in the kind inference constructor.
dropTvbKind :: DTyVarBndr -> DTyVarBndr
dropTvbKind tvb@(DPlainTV {}) = tvb
dropTvbKind (DKindedTV n _)   = DPlainTV n

-- Shorthand for building (k1 ~> k2)
buildTyFunArrow :: DKind -> DKind -> DKind
buildTyFunArrow k1 k2 = DConT tyFunArrowName `DAppT` k1 `DAppT` k2

buildTyFunArrow_maybe :: Maybe DKind -> Maybe DKind -> Maybe DKind
buildTyFunArrow_maybe m_k1 m_k2 = do
  k1 <- m_k1
  k2 <- m_k2
  return $ DConT tyFunArrowName `DAppT` k1 `DAppT` k2

-- Build (~>) kind from the list of kinds
ravelTyFun :: [DKind] -> DKind
ravelTyFun []    = error "Internal error: TyFun raveling nil"
ravelTyFun [k]   = k
ravelTyFun kinds = go tailK (buildTyFunArrow k2 k1)
    where (k1 : k2 : tailK) = reverse kinds
          go []     acc = acc
          go (k:ks) acc = go ks (buildTyFunArrow k acc)

{-
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
