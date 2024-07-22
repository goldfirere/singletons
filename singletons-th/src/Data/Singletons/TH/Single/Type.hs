{- Data/Singletons/TH/Single/Type.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

Singletonizes types.
-}

module Data.Singletons.TH.Single.Type where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Promote.Type
import Data.Singletons.TH.Single.Monad
import Data.Singletons.TH.Util
import Control.Monad

singType :: DType          -- the promoted version of the thing classified by...
         -> DType          -- ... this type
         -> SgM ( DType    -- the singletonized type
                , Int      -- the number of arguments
                , [Name]   -- the names of the tyvars used in the sing'd type
                , DCxt     -- the context of the singletonized type
                , [DKind]  -- the kinds of the argument types
                , DKind )  -- the kind of the result type
singType prom ty = do
  (orig_tvbs, cxt, args, res) <- unravelVanillaDType ty
  let num_args = length args
  cxt' <- mapM singPred_NC cxt
  arg_names <- replicateM num_args (qNewName "t")
  prom_args <- mapM promoteType_NC args
  prom_res  <- promoteType_NC res
  let args' = map (\n -> singFamily `DAppT` (DVarT n)) arg_names
      res'  = singFamily `DAppT` (foldType prom (map DVarT arg_names) `DSigT` prom_res)
                -- Make sure to include an explicit `prom_res` kind annotation.
                -- See Note [Preserve the order of type variables during singling],
                -- wrinkle 2.
      arg_tvbs = zipWith (`DKindedTV` SpecifiedSpec) arg_names prom_args
      -- If the original type signature lacks an explicit `forall`, then do not
      -- give the singled type signature an outermost `forall`. Instead, give it
      -- a `<singled-ty> :: Type` kind annotation and let GHC implicitly
      -- quantify any type variables that are free in `<singled-ty>`.
      -- See Note [Preserve the order of type variables during singling],
      -- wrinkle 1.
      ty' | null orig_tvbs
          = ravelVanillaDType arg_tvbs cxt' args' res' `DSigT` DConT typeKindName
          | otherwise
          = ravelVanillaDType (orig_tvbs ++ arg_tvbs) cxt' args' res'
  return (ty', num_args, arg_names, cxt, prom_args, prom_res)

-- Single a DPred, checking that it is a vanilla type in the process.
-- See [Vanilla-type validity checking during promotion]
-- in Data.Singletons.TH.Promote.Type.
singPred :: DPred -> SgM DPred
singPred p = do
  checkVanillaDType p
  singPred_NC p

-- Single a DPred. Does not check if the argument is a vanilla type.
-- See [Vanilla-type validity checking during promotion]
-- in Data.Singletons.TH.Promote.Type.
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
singletons-th does its best to preseve the order in which users write type
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
by their left-to-right order of appearance in the type signature. This order
dictates that `a` is quantified before `b`, so we must mirror this order in the
singled type signature.

One way to accomplish this would be to compute the order in which the type
variables appear and then explicitly quantify them. In the `absurd` example
above, this would be tantamount to writing:

  sAbsurd :: forall a b (v :: V a). Sing v -> Sing (Absurd v :: b)
                    ^^^
                    |||
         Explicitly quantified by singletons-th,
         not in the original type signature

This is possible to do, and indeed, singletons-th used to do this. However, it
is a bit tiresome to implement. In order to know which type variables to
quantify, you must keep track of which type variables have been brought into
scope at all times. For the historical details on how this worked, see this
now-removed Note describing the old implementation:
https://github.com/goldfirere/singletons/blob/10ef27880d7ecc16241824c504ca83e2bb6ca787/singletons-th/src/Data/Singletons/TH/Promote/Monad.hs#L135-L192

A much more straightforward approach, which singletons-th currently uses, is to
let GHC do the hard work of implicitly quantifying the type variables. That is,
we will single `absurd` to something like this:

  sAbsurd :: () => forall (v :: V a). Sing v -> Sing (Absurd v :: b)

This works because just like in the original type signature, `a` and `b` are
implicitly quantified, and more importantly, they are quantified in exactly the
same order as in the original type signature.

Why do we need the `() => ...` part? If we had instead written the type
signature like this:

  sAbsurd :: forall (v :: V a). Sing v -> Sing (Absurd v :: b)

Then GHC would reject `a` and `b` for being out of scope. This is because of
GHC's "forall-or-nothing" rule: if a type signature has an outermost forall,
then all type variable occurrences in the type signature must have explicit
binding sites. Using `() => forall (v :: V a). ...` prevents the `forall` from
being an outermost `forall`, which bypasses the forall-or-nothing rule.

Some further complications:

* Template Haskell doesn't actually allow you to splice in types of the form
  `() => ...` in practice.
  See https://gitlab.haskell.org/ghc/ghc/-/issues/16396. Luckily, this isn't a
  deal-breaker, as we can also avoid the forall-or-nothing rule by annotating
  the type signature with an explicit `... :: Type` annotation:

    sAbsurd :: ((forall (v :: V a). Sing v -> Sing (Absurd v :: b)) :: Type)

  This is the approach that singletons-th actually uses. Note that there is one
  spot in the code (in D.S.TH.Single.singInstD) that must be taught to look
  through these `... :: Type` annotations, but this approach is otherwise fairly
  non-invasive.

* We cannot use this trick when singling the types of data constructors. That
  is, we cannot single this:

    data T a where
      MkT :: a -> T a

  To this:

    data ST z where
      SMkT :: ((forall (x :: a). Sing x -> ST (MkT x)) :: Type)

  This is because GADT syntax does not currently permit nested `forall`s of this
  sort. (It might permit them in the future if
  https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0402-gadt-syntax.rst
  is implemented, but not currently.) As a result, we /always/ explicitly
  quantify all type variables in a data constructor's type, regardless of
  whether the original type implicitly quantified them or not. In the example
  above, that means that the singled version would be:

    data ST z where
      SMkT :: forall a (x :: a). Sing x -> ST (MkT x)

-----
-- Wrinkle 2: Where to put explicit kind annotations
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
approach fails. Suppose that `Nothing` was declared like so:

  Nothing :: forall {a}. Maybe a

Then we couldn't write `Nothing @a`, as `a` wouldn't be eligible for visible
kind application. (GHC Core would let you write `Nothing @{a}`, but this isn't
possible in source Haskell.) As such, the only way to fix the type of `a` in
`Maybe a` would be to write `Nothing :: Maybe a`.

An even more obscure corner case is when a type quantifies a type variable
without mentioning it at all in the rest of the type, e.g.,

  C :: forall a. D

At present, singletons-th does not handle this type of corner case at all. It
wouldn't suffice to single `C` to the following:

  SC :: forall a. SD (C :: D)

This is because the `a` argument to `C` is left ambiguous in the type `C :: D`,
as the return kind `D` doesn't indicate what `a` should be. In theory, we
/could/ write `C @a :: D` instead, which would solve that particular problem.
But there would still be trouble on the horizon, as there are other types of
code that singletons-th generates which exhibit similar sorts of ambiguity. For
example, here is the defunctionalization symbol that singletons-th would
generate for `C`:

  type CSym0 :: forall a. D
  type family CSym0 @a :: D where
    CSym0 = C

GHC will reject this type family equation, also for ambiguity-related reasons:

  error: [GHC-16220]
      • Uninferrable type variables k0, (a0 :: k0) in
        the type family equation right-hand side: C @{k0} @a0
      • In the type family declaration for ‘CSym0’

This is fixable, but we would need to audit all potential sources of type
variable-related ambiguity in singletons-th–generated code and make sure that
we have covered our bases. Given that types such as `forall a. D` are rare in
practice, I'm inclined not to worry about this unless someone specifically asks
for this featurette.

Finally, note that we need only write `Sing x_1 -> ... -> Sing x_p`, and not
`Sing (x_1 :: PT_1) -> ... Sing (x_p :: PT_p)`. This is simply because we
always use explicit `forall`s in singled type signatures, and therefore always
explicitly bind `(x_1 :: PT_1) ... (x_p :: PT_p)`, which fully determine the
kinds of `x_1 ... x_p`. It wouldn't be wrong to add extra kind annotations to
`Sing x_1 -> ... -> Sing x_p`, just redundant.
-}
