{- Data/Singletons/Single/Type.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

Singletonizes types.
-}

module Data.Singletons.Single.Type where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.OSet (OSet)
import Language.Haskell.TH.Syntax
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.Promote.Type
import Data.Singletons.TH.Options
import Data.Singletons.Util
import Control.Monad
import Data.Foldable
import Data.Function
import Data.List (deleteFirstsBy)

singType :: OSet Name      -- the set of bound kind variables in this scope
                           -- see Note [Explicitly binding kind variables]
                           -- in Data.Singletons.Promote.Monad
         -> DType          -- the promoted version of the thing classified by...
         -> DType          -- ... this type
         -> SgM ( DType    -- the singletonized type
                , Int      -- the number of arguments
                , [Name]   -- the names of the tyvars used in the sing'd type
                , DCxt     -- the context of the singletonized type
                , [DKind]  -- the kinds of the argument types
                , DKind )  -- the kind of the result type
singType bound_kvs prom ty = do
  (orig_tvbs, cxt, args, res) <- unravelVanillaDType ty
  let num_args = length args
  cxt' <- mapM singPred_NC cxt
  arg_names <- replicateM num_args (qNewName "t")
  prom_args <- mapM promoteType_NC args
  prom_res  <- promoteType_NC res
  let args' = map (\n -> singFamily `DAppT` (DVarT n)) arg_names
      res'  = singFamily `DAppT` (foldl apply prom (map DVarT arg_names) `DSigT` prom_res)
                -- Make sure to include an explicit `prom_res` kind annotation.
                -- See Note [Preserve the order of type variables during singling],
                -- wrinkle 3.
      kvbs     = singTypeKVBs orig_tvbs prom_args cxt' prom_res bound_kvs
      all_tvbs = kvbs ++ zipWith DKindedTV arg_names prom_args
      ty'      = ravelVanillaDType all_tvbs cxt' args' res'
  return (ty', num_args, arg_names, cxt, prom_args, prom_res)

-- Compute the kind variable binders to use in the singled version of a type
-- signature. This has two main call sites: singType and D.S.Single.Data.singCtor.
--
-- This implements the advice documented in
-- Note [Preserve the order of type variables during singling], wrinkle 1.
singTypeKVBs ::
     [DTyVarBndr] -- ^ The bound type variables from the original type signature.
  -> [DType]      -- ^ The argument types of the signature (promoted).
  -> DCxt         -- ^ The context of the signature (singled).
  -> DType        -- ^ The result type of the signature (promoted).
  -> OSet Name    -- ^ The type variables previously bound in the current scope.
  -> [DTyVarBndr] -- ^ The kind variables for the singled type signature.
singTypeKVBs orig_tvbs prom_args sing_ctxt prom_res bound_tvbs
  | null orig_tvbs
  -- There are no explicitly `forall`ed type variable binders, so we must
  -- infer them ourselves.
  = deleteFirstsBy
      ((==) `on` extractTvbName)
      (toposortTyVarsOf $ prom_args ++ sing_ctxt ++ [prom_res])
      (map DPlainTV $ toList bound_tvbs)
      -- Make sure to subtract out the bound variables currently in scope,
      -- lest we accidentally shadow them in this type signature.
      -- See Note [Explicitly binding kind variables] in D.S.Promote.Monad.
  | otherwise
  -- There is an explicit `forall`, so this case is easy.
  = orig_tvbs

-- Single a DPred, checking that it is a vanilla type in the process.
-- See [Vanilla-type validity checking during promotion]
-- in Data.Singletons.Promote.Type.
singPred :: DPred -> SgM DPred
singPred p = do
  checkVanillaDType p
  singPred_NC p

-- Single a DPred. Does not check if the argument is a vanilla type.
-- See [Vanilla-type validity checking during promotion]
-- in Data.Singletons.Promote.Type.
singPred_NC :: DPred -> SgM DPred
singPred_NC = singPredRec []

-- The workhorse for singPred_NC.
singPredRec :: [DTypeArg] -> DPred -> SgM DPred
singPredRec _cxt (DForallT {}) =
  fail "Singling of quantified constraints not yet supported"
singPredRec _cxt (DConstrainedT {}) =
  fail "Singling of quantified constraints not yet supported"
singPredRec ctx (DAppT pr ty) = singPredRec (DTANormal ty : ctx) pr
singPredRec ctx (DAppKindT pr ki) = singPredRec (DTyArg ki : ctx) pr
singPredRec _ctx (DSigT _pr _ki) =
  fail "Singling of constraints with explicit kinds not yet supported"
singPredRec _ctx (DVarT _n) =
  fail "Singling of contraint variables not yet supported"
singPredRec ctx (DConT n)
  | n == equalityName
  = fail "Singling of type equality constraints not yet supported"
  | otherwise = do
    opts <- getOptions
    kis <- mapM promoteTypeArg_NC ctx
    let sName = singledClassName opts n
    return $ applyDType (DConT sName) kis
singPredRec _ctx DWildCardT = return DWildCardT  -- it just might work
singPredRec _ctx DArrowT =
  fail "(->) spotted at head of a constraint"
singPredRec _ctx (DLitT {}) =
  fail "Type-level literal spotted at head of a constraint"

{-
Note [Preserve the order of type variables during singling]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
singletons does its best to preseve the order in which users write type
variables in type signatures for functions and data constructors. They are
"preserved" in the sense that if one writes `foo @T1 @T2`, one should be
able to write out `sFoo @T1 @T2` by hand and have the same order of visible
type applications still work. Accomplishing this is surprisingly nontrivial,
so this Note documents the various wrinkles one must iron out to get this
working.

-----
-- Wrinkle 1: Dealing with the presence (and absence) of `forall`
-----

If we single a function that has an explicit `forall`, such as this example:

  const2 :: forall b a. a -> b -> a
  const2 x _ = x

Then our job is easy, as the exact order of type variables has already been
spelled out in advance. We single this to:

  sConst2 :: forall b a (x :: a) (y :: b). Sing x -> Sing y -> Sing (Const2 x y :: a)
  sConst2 = ...

What happens if there is no explicit `forall`, as in this example?

  data V a

  absurd :: V a -> b
  absurd v = case v of {}

This time, the order of type variables vis-à-vis TypeApplications is determined
by their left-to-right order of appearance in the type signature. It's tempting
to think that since there is no explicit `forall` in the original type
signature, we could get away without an explicit `forall` in the singled type
signature. That is, one could write:

  sAbsurd :: Sing (v :: V a) -> Sing (Absurd :: b)

This would have the right type variable order, but unfortunately, this approach
does not play well with singletons' style of code generation. Consider the code
that would be generated for the body of sAbsurd:

  sAbsurd :: Sing (v :: V a) -> Sing (Absurd :: b)
  sAbsurd (sV :: Sing v) = id @(Case v v :: b) (case sV of {})

Note the use of the type `Case v v :: b` in the right-hand side of sAbsurd.
However, because `b` was not bound by a top-level `forall`, it won't be in
scope here, resulting in an error!

(Why do we generate the code `id @(Case v v :: b)` in the first place? See
Note [The id hack; or, how singletons learned to stop worrying and avoid kind generalization]
in D.S.Single.)

The simplest approach is to just always generate singled type signatures with
explicit `forall`s. In the event that the original type signature lacks an
explicit `forall`, we inferr the correct type variable ordering ourselves and
synthesize a `forall` with that order. The `singTypeKVBs` function implements
this logic.

-----
-- Wrinkle 2: The TH reification swamp
-----

There is another issue with type signatures that lack explicit `forall`s, one
which the current design of Template Haskell does not make simple to fix.
If we single code that is wrapped in TH quotes, such as in the following example:

  $(singletons [d|
    data Proxy (a :: k) where
      MkProxy :: Proxy a
    |])

Then our job is made much easier when singling MkProxy, since we know that the
only type variable that must be quantified is `a`, as that is the only one
specified in the type signature.

However, this is not the only possible way to single MkProxy. One can
alternatively use $(genSingletons [''Proxy]), which uses TH reification to
infer the type of MkProxy. There is perilous, however, because this is how
TH reifies MkProxy:

  ForallC [KindedTV k StarT,KindedTV a (VarT k)] []
          (GadtC [MkProxy] [] (AppT (ConT Proxy) (VarT a)))

In terms of actual Haskell code, that's:

  MkProxy :: forall k (a :: k). Proxy a

This is subtly different than before, as `k` is now specified. Contrast this
with `MkProxy :: Proxy a`, where `k` is invisible. In other words, if you
single MkProxy using genSingletons, then `Proxy @True` will typecheck but
`SMkProxy @True` will /not/ typecheck—you'd have to use `SMkProxy @_ @True`
instead. Urk!

At present, Template Haskell does not have a way to distinguish specified from
inferred type variables—see GHC #17159—and it is unclear how one could work
around this issue withouf first fixing #17159 upstream. Thankfully, it is
only likely to bite in situations where the original type signature uses
inferred variables, so the damage is somewhat minimal.

-----
-- Wrinkle 3: Where to put explicit kind annotations
-----

Type variable binders are only part of the story—we must also determine what
the body of the type signature will be singled to. As a general rule, if the
original type signature is of the form:

  f :: forall a_1 ... a_m. (C_1, ..., C_n)
    => T_1 -> ... -> T_p -> R

Then the singled type signature will be:

  sF :: forall a_1 ... a_m (x_1 :: PT_1) ... (x_p :: PT_p). (SC_1, ..., SC_n)
     => Sing x1 -> ... -> Sing x_p -> SRes (F x1 ... x_p :: PR)

Where:

* x_i is a fresh type variable of kind PT_i.
* PT_i is the promoted version of the type T_i, and PR is the promoted version
  of the type R.
* SC_i is the singled version of the constraint SC_i.
* SRes is either `Sing` if dealing with a function, or a singled data type if
  dealing with a data constructor. For instance, SRes is `SBool` in
  `STrue :: SBool (True :: Bool)`.

One aspect of this worth pointing out is the explicit `:: PR` kind annotation
in the result type `Sing (F x1 ... x_p :: PR)`. As it turns out, this kind
annotation is mandatory, as omitting can result in singled type signatures
with the wrong semantics. For instance, consider the `Nothing` data
constructor:

  Nothing :: forall a. Maybe a

Consider what would happen if it were singled to this type:

  SNothing :: forall a. SMaybe Nothing

This is not what we want at all, since the `a` has no connection to the
`Nothing` in the result type. It's as if we had written this:

  SNothing :: forall {t} a. SMaybe (Nothing :: Maybe t)

If we instead generate `forall a. SMaybe (Nothing :: Maybe a)`, then this issue
is handily avoided.

You might wonder if it would be cleaner to use visible kind applications
instead:

  SNothing :: forall a. SMaybe (Nothing @a)

This does work for many cases, but there are also some corner cases where this
approach fails. Recall the `MkProxy` example from Wrinkle 2 above:

  data Proxy (a :: k) where
    MkProxy :: Proxy a
  $(genSingletons [''Proxy])

Due to the design of Template Haskell (discussed in Wrinkle 2), `MkProxy` will
be reified with the type of `forall k (a :: k). Proxy a`. This means that
if we used visible kind applications in the result type, we would end up with
this:

  SMkProxy :: forall k (a :: k). SProxy (MkProxy @k @a)

This will not kind-check because MkProxy only accepts /one/ visible kind argument,
whereas this supplies it with two. To avoid this issue, we instead use the type
`forall k (a :: k). SProxy (MkProxy :: Proxy a)`. Granted, this type is /still/
technically wrong due to the fact that it explicitly quantifies `k`, but at the
very least it typechecks. If GHC #17159 were fixed, we could revisit this
design choice.

Finally, note that we need only write `Sing x_1 -> ... -> Sing x_p`, and not
`Sing (x_1 :: PT_1) -> ... Sing (x_p :: PT_p)`. This is simply because we
always use explicit `forall`s in singled type signatures, and therefore always
explicitly bind `(x_1 :: PT_1) ... (x_p :: PT_p)`, which fully determine the
kinds of `x_1 ... x_p`. It wouldn't be wrong to add extra kind annotations to
`Sing x_1 -> ... -> Sing x_p`, just redundant.
-}
