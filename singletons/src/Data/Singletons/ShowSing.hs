{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.ShowSing
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the class 'ShowSing' which is useful for defining 'Show' instances
-- for singleton types. Because 'ShowSing' crucially relies on
-- @QuantifiedConstraints@, it is only defined if this library is built with
-- GHC 8.6 or later.
--
----------------------------------------------------------------------------

module Data.Singletons.ShowSing (
#if __GLASGOW_HASKELL__ >= 806
  -- * The 'ShowSing' type
  ShowSing,

  -- * Internal utilities
  ShowSing'
#endif
  ) where

#if __GLASGOW_HASKELL__ >= 806
import Data.Kind
import Data.Singletons
import Text.Show

-- | In addition to the promoted and singled versions of the 'Show' class that
-- @singletons-base@ provides, it is also useful to be able to directly define
-- 'Show' instances for singleton types themselves. Doing so is almost entirely
-- straightforward, as a derived 'Show' instance does 90 percent of the work.
-- The last 10 percent—getting the right instance context—is a bit tricky, and
-- that's where 'ShowSing' comes into play.
--
-- As an example, let's consider the singleton type for lists. We want to write
-- an instance with the following shape:
--
-- @
-- instance ??? => 'Show' ('SList' (z :: [k])) where
--   showsPrec p 'SNil' = showString \"SNil\"
--   showsPrec p ('SCons' sx sxs) =
--     showParen (p > 10) $ showString \"SCons \" . showsPrec 11 sx
--                        . showSpace . showsPrec 11 sxs
-- @
--
-- To figure out what should go in place of @???@, observe that we require the
-- type of each field to also be 'Show' instances. In other words, we need
-- something like @('Show' ('Sing' (a :: k)))@. But this isn't quite right, as the
-- type variable @a@ doesn't appear in the instance head. In fact, this @a@
-- type is really referring to an existentially quantified type variable in the
-- 'SCons' constructor, so it doesn't make sense to try and use it like this.
--
-- Luckily, the @QuantifiedConstraints@ language extension provides a solution
-- to this problem. This lets you write a context of the form
-- @(forall a. 'Show' ('Sing' (a :: k)))@, which demands that there be an instance
-- for @'Show' ('Sing' (a :: k))@ that is parametric in the use of @a@.
-- This lets us write something closer to this:
--
-- @
-- instance (forall a. 'Show' ('Sing' (a :: k))) => 'SList' ('Sing' (z :: [k])) where ...
-- @
--
-- The 'ShowSing' class is a thin wrapper around
-- @(forall a. 'Show' ('Sing' (a :: k)))@. With 'ShowSing', our final instance
-- declaration becomes this:
--
-- @
-- instance 'ShowSing' k => 'Show' ('SList' (z :: [k])) where ...
-- @
--
-- In fact, this instance can be derived:
--
-- @
-- deriving instance 'ShowSing' k => 'Show' ('SList' (z :: [k]))
-- @
--
-- (Note that the actual definition of 'ShowSing' is slightly more complicated
-- than what this documentation might suggest. For the full story,
-- refer to the documentation for `ShowSing'`.)
--
-- When singling a derived 'Show' instance, @singletons-th@ will also generate
-- a 'Show' instance for the corresponding singleton type using 'ShowSing'.
-- In other words, if you give @singletons-th@ a derived 'Show' instance, then
-- you'll receive the following in return:
--
-- * A promoted (@PShow@) instance
-- * A singled (@SShow@) instance
-- * A 'Show' instance for the singleton type
--
-- What a bargain!

-- One might wonder we we simply don't define ShowSing as
-- @type ShowSing k = (forall (z :: k). ShowSing' z)@ instead of going the
-- extra mile to define it as a class.
-- See Note [Define ShowSing as a class, not a type synonym] for an explanation.
#if __GLASGOW_HASKELL__ >= 810
type ShowSing :: Type -> Constraint
#endif
class    (forall (z :: k). ShowSing' z) => ShowSing (k :: Type)
instance (forall (z :: k). ShowSing' z) => ShowSing (k :: Type)

-- | The workhorse that powers 'ShowSing'. The only reason that `ShowSing'`
-- exists is to work around GHC's inability to put type families in the head
-- of a quantified constraint (see
-- <https://gitlab.haskell.org/ghc/ghc/issues/14860 this GHC issue> for more
-- details on this point). In other words, GHC will not let you define
-- 'ShowSing' like so:
--
-- @
-- class (forall (z :: k). 'Show' ('Sing' z)) => 'ShowSing' k
-- @
--
-- By replacing @'Show' ('Sing' z)@ with @ShowSing' z@, we are able to avoid
-- this restriction for the most part.
--
-- The superclass of `ShowSing'` is a bit peculiar:
--
-- @
-- class (forall (sing :: k -> Type). sing ~ 'Sing' => 'Show' (sing z)) => `ShowSing'` (z :: k)
-- @
--
-- One might wonder why this superclass is used instead of this seemingly more
-- direct equivalent:
--
-- @
-- class 'Show' ('Sing' z) => `ShowSing'` (z :: k)
-- @
--
-- Actually, these aren't equivalent! The latter's superclass mentions a type
-- family in its head, and this gives GHC's constraint solver trouble when
-- trying to match this superclass against other constraints. (See the
-- discussion beginning at
-- https://gitlab.haskell.org/ghc/ghc/-/issues/16365#note_189057 for more on
-- this point). The former's superclass, on the other hand, does /not/ mention
-- a type family in its head, which allows it to match other constraints more
-- easily. It may sound like a small difference, but it's the only reason that
-- 'ShowSing' is able to work at all without a significant amount of additional
-- workarounds.
--
-- The quantified superclass has one major downside. Although the head of the
-- quantified superclass is more eager to match, which is usually a good thing,
-- it can bite under certain circumstances. Because @'Show' (sing z)@ will
-- match a 'Show' instance for /any/ types @sing :: k -> Type@ and @z :: k@,
-- (where @k@ is a kind variable), it is possible for GHC's constraint solver
-- to get into a situation where multiple instances match @'Show' (sing z)@,
-- and GHC will get confused as a result. Consider this example:
--
-- @
-- -- As in "Data.Singletons"
-- newtype 'WrappedSing' :: forall k. k -> Type where
--   'WrapSing' :: forall k (a :: k). { 'unwrapSing' :: 'Sing' a } -> 'WrappedSing' a
--
-- instance 'ShowSing' k => 'Show' ('WrappedSing' (a :: k)) where
--   'showsPrec' _ s = 'showString' "WrapSing {unwrapSing = " . showsPrec 0 s . showChar '}'
-- @
--
-- When typechecking the 'Show' instance for 'WrappedSing', GHC must fill in a
-- default definition @'show' = defaultShow@, where
-- @defaultShow :: 'Show' ('WrappedSing' a) => 'WrappedSing' a -> 'String'@.
-- GHC's constraint solver has two possible ways to satisfy the
-- @'Show' ('WrappedSing' a)@ constraint for @defaultShow@:
--
-- 1. The top-level instance declaration for @'Show' ('WrappedSing' (a :: k))@
--    itself, and
--
-- 2. @'Show' (sing (z :: k))@ from the head of the quantified constraint arising
--    from @'ShowSing' k@.
--
-- In practice, GHC will choose (2), as local quantified constraints shadow
-- global constraints. This confuses GHC greatly, causing it to error out with
-- an error akin to @Couldn't match type Sing with WrappedSing@. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/17934 for a full diagnosis of
-- the issue.
--
-- The bad news is that because of GHC#17934, we have to manually define 'show'
-- (and 'showList') in the 'Show' instance for 'WrappedSing' in order to avoid
-- confusing GHC's constraint solver. In other words, @deriving 'Show'@ is a
-- no-go for 'WrappedSing'. The good news is that situations like 'WrappedSing'
-- are quite rare in the world of @singletons@—most of the time, 'Show'
-- instances for singleton types do /not/ have the shape
-- @'Show' (sing (z :: k))@, where @k@ is a polymorphic kind variable. Rather,
-- most such instances instantiate @k@ to a specific kind (e.g., @Bool@, or
-- @[a]@), which means that they will not overlap the head of the quantified
-- superclass in `ShowSing'` as observed above.
--
-- Note that we define the single instance for `ShowSing'` without the use of a
-- quantified constraint in the instance context:
--
-- @
-- instance 'Show' ('Sing' z) => `ShowSing'` (z :: k)
-- @
--
-- We /could/ define this instance with a quantified constraint in the instance
-- context, and it would be equally as expressive. But it doesn't provide any
-- additional functionality that the non-quantified version gives, so we opt
-- for the non-quantified version, which is easier to read.
#if __GLASGOW_HASKELL__ >= 810
type ShowSing' :: k -> Constraint
#endif
class    (forall (sing :: k -> Type). sing ~ Sing => Show (sing z))
                       => ShowSing' (z :: k)
instance Show (Sing z) => ShowSing' (z :: k)

{-
Note [Define ShowSing as a class, not a type synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In an ideal world, we would simply define ShowSing like this:

  type ShowSing k = (forall (z :: k). ShowSing' z) :: Constraint)

In fact, I used to define ShowSing in a manner similar to this in version 2.5
of singletons. However, I realized some time after 2.5's release that the
this encoding is unfeasible at the time being due to GHC Trac #15888.

To be more precise, the exact issue involves an infelicity in the way
QuantifiedConstraints interacts with recursive type class instances.
Consider the following example (from #371):

  $(singletons [d|
    data X a = X1 | X2 (Y a) deriving Show
    data Y a = Y1 | Y2 (X a) deriving Show
    |])

This will generate the following instances:

  deriving instance ShowSing (Y a) => Show (Sing (z :: X a))
  deriving instance ShowSing (X a) => Show (Sing (z :: Y a))

So far, so good. Now, suppose you try to actually `show` a singleton for X.
For example:

  show (sing @(X1 :: X Bool))

Somewhat surprisingly, this will be rejected by the typechecker with the
following error:

    • Reduction stack overflow; size = 201
      When simplifying the following type: Show (Sing z)

To see why this happens, observe what goes on if we expand the occurrences of
the ShowSing type synonym in the generated instances:

  deriving instance (forall z. ShowSing' (z :: Y a)) => Show (Sing (z :: X a))
  deriving instance (forall z. ShowSing' (z :: X a)) => Show (Sing (z :: Y a))

Due to the way QuantifiedConstraints currently works (as surmised in Trac
#15888), when GHC has a Wanted `ShowSing' (X1 :: X Bool)` constraint, it
chooses the appropriate instance and emits a Wanted
`forall z. ShowSing' (z :: Y Bool)` constraint (from the instance context).
GHC skolemizes the `z` to `z1` and tries to solve a Wanted
`ShowSing' (z1 :: Y Bool)` constraint. GHC chooses the appropriate instance
and emits a Wanted `forall z. ShowSing' (z :: X Bool)` constraint. GHC
skolemizes the `z` to `z2` and tries to solve a Wanted
`ShowSing' (z2 :: X Bool)` constraint... we repeat the process and find
ourselves in an infinite loop that eventually overflows the reduction stack.
Eep.

Until Trac #15888 is fixed, there are two possible ways to work around this
problem:

1. Make derived instances' type inference more clever. If you look closely,
   you'll notice that the `ShowSing (X a)`/`ShowSing (Y a)` constraints in
   the generated instances are entirely redundant and could safely be left
   off. But determining this would require significantly improving singletons-th'
   Template Haskell capabilities for type inference, which is a path that we
   usually spurn in favor of keeping the generated code dumb but predictable.
2. Define `ShowSing` as a class (with a single instance) instead of a type
   synonym. `ShowSing`-as-a-class ties the recursive knot during instance
   resolution and thus avoids the problems that the type synonym version
   currently suffers from.

Given the two options, (2) is by far the easier option, so that is what we
ultimately went with.
-}

------------------------------------------------------------
-- (S)WrappedSing instances
------------------------------------------------------------

-- Note that we cannot derive this Show instance due to
-- https://gitlab.haskell.org/ghc/ghc/-/issues/17934. The Haddocks for
-- ShowSing' contain a lengthier explanation of how GHC#17934 relates to
-- ShowSing.
instance ShowSing k => Show (WrappedSing (a :: k)) where
  showsPrec = showsWrappedSingPrec
  show x = showsWrappedSingPrec 0 x ""
  showList = showListWith (showsWrappedSingPrec 0)

showsWrappedSingPrec :: ShowSing k => Int -> WrappedSing (a :: k) -> ShowS
showsWrappedSingPrec p (WrapSing s) = showParen (p >= 11) $
  showString "WrapSing {unwrapSing = " . showsPrec 0 s . showChar '}'

deriving instance ShowSing k => Show (SWrappedSing (ws :: WrappedSing (a :: k)))
#endif
