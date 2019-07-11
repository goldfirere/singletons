{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.ShowSing
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the class 'ShowSing' type synonym, which is useful for defining
-- 'Show' instances for singleton types.
--
----------------------------------------------------------------------------

module Data.Singletons.ShowSing (
  -- * The 'ShowSing' type
  ShowSing,

  -- * Internal utilities
  ShowSing'
  ) where

import Data.Singletons.Internal
import Data.Singletons.Prelude.Instances
import Data.Singletons.Single
import Data.Singletons.TypeLits.Internal
import Data.Singletons.Util
import GHC.Show (appPrec, appPrec1)
import qualified GHC.TypeNats as TN

-- | In addition to the promoted and singled versions of the 'Show' class that
-- @singletons@ provides, it is also useful to be able to directly define
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
-- instance 'ShowSing' k => 'Show' ('SList' (z :: [k])) where
--   showsPrec p 'SNil' = showString \"SNil\"
--   showsPrec p ('SCons' (sx :: 'Sing' x) (sxs :: 'Sing' xs)) =
--     (showParen (p > 10) $ showString \"SCons \" . showsPrec 11 sx
--                         . showSpace . showsPrec 11 sxs)
--       :: (ShowSing' x, ShowSing' xs) => ShowS
-- @
--
-- (Note that the actual definition of 'ShowSing' is slightly more complicated
-- than what this documentation might suggest. For the full story, as well as
-- an explanation of why we need an explicit
-- @(ShowSing' x, ShowSing' xs) => ShowS@ signature at the end,
-- refer to the documentation for `ShowSing'`.)
--
-- When singling a derived 'Show' instance, @singletons@ will also generate
-- a 'Show' instance for the corresponding singleton type using 'ShowSing'.
-- In other words, if you give @singletons@ a derived 'Show' instance, then
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
class    (forall (z :: k). ShowSing' z) => ShowSing k
instance (forall (z :: k). ShowSing' z) => ShowSing k

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
-- this restriction for the most part. There is one major downside to using
-- @ShowSing'@, however: deriving 'Show' instances for singleton types does
-- not work out of the box. In other words, if you try to do this:
--
-- @
-- deriving instance 'ShowSing' k => 'Show' ('SList' (z :: [k]))
-- @
--
-- Then GHC will complain to the effect that it could not deduce a
-- @'Show' ('Sing' x)@ constraint. This is due to
-- <https://gitlab.haskell.org/ghc/ghc/issues/16365 another unfortunate GHC bug>
-- that prevents GHC from realizing that @'ShowSing' k@ implies
-- @'Show' ('Sing' (x :: k))@. The workaround is to force GHC to come to its
-- senses by using an explicit type signature:
--
-- @
-- instance 'ShowSing' k => 'Show' ('SList' (z :: [k])) where
--   showsPrec p 'SNil' = showString \"SNil\"
--   showsPrec p ('SCons' (sx :: 'Sing' x) (sxs :: 'Sing' xs)) =
--     (showParen (p > 10) $ showString \"SCons \" . showsPrec 11 sx
--                         . showSpace . showsPrec 11 sxs)
--       :: (ShowSing' x, ShowSing' xs) => ShowS
-- @
--
-- The use of @ShowSing' x@ in the signature is sufficient to make the
-- constraint solver connect the dots between @'ShowSing' k@ and
-- @'Show' ('Sing' (x :: k))@. (The @ShowSing' xs@ constraint is not strictly
-- necessary, but it is shown here since that is in fact the code that
-- @singletons@ will generate for this instance.)
--
-- Because @deriving 'Show'@ will not insert these explicit signatures for us,
-- it is not possible to derive 'Show' instances for singleton types.
-- Thankfully, @singletons@' Template Haskell machinery can do this manual
-- gruntwork for us 99% of the time, but if you ever find yourself in a
-- situation where you must define a 'Show' instance for a singleton type by
-- hand, this is important to keep in mind.
--
-- Note that there is one potential future direction that might alleviate this
-- pain. We could define `ShowSing'` like this instead:
--
-- @
-- class (forall sing. sing ~ 'Sing' => 'Show' (sing z)) => ShowSing' z
-- instance 'Show' ('Sing' z) => ShowSing' z
-- @
--
-- For many examples, this lets you just derive 'Show' instances for singleton
-- types like you would expect. Alas, this topples over on @Bar@ in the
-- following example:
--
-- @
-- newtype Foo a = MkFoo a
-- data SFoo :: forall a. Foo a -> Type where
--   SMkFoo :: Sing x -> SFoo (MkFoo x)
-- type instance Sing = SFoo
-- deriving instance ShowSing a => Show (SFoo (z :: Foo a))
--
-- newtype Bar a = MkBar (Foo a)
-- data SBar :: forall a. Bar a -> Type where
--   SMkBar :: Sing x -> SBar (MkBar x)
-- type instance Sing = SBar
-- deriving instance ShowSing (Foo a) => Show (SBar (z :: Bar a))
-- @
--
-- This fails because
-- of—you guessed it—<https://gitlab.haskell.org/ghc/ghc/issues/16502 another GHC bug>.
-- Bummer. Unless that bug were to be fixed, the current definition of
-- `ShowSing'` is the best that we can do.
class    Show (Sing z) => ShowSing' z
instance Show (Sing z) => ShowSing' z

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
   off. But determining this would require significantly improving singletons'
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

instance ShowSing k => Show (WrappedSing (a :: k)) where
  showsPrec p (WrapSing s) = showParen (p >= 11) $
    showString "WrapSing {unwrapSing = " . showsPrec 0 s . showChar '}'
      :: ShowSing' a => ShowS

instance ShowSing k => Show (SWrappedSing (ws :: WrappedSing (a :: k))) where
  showsPrec p (SWrapSing s) = showParen (p >= 11) $
    showString "SWrapSing {sUnwrapSing = " . showsPrec 0 s . showChar '}'
      :: ShowSing' a => ShowS

------------------------------------------------------------
-- TypeLits instances
------------------------------------------------------------

-- These are a bit special because the singleton constructor does not uniquely
-- determine the type being used in the constructor's return type (e.g., all Nats
-- have the same singleton constructor, SNat). To compensate for this, we display
-- the type being used using visible type application. (Thanks to @cumber on #179
-- for suggesting this implementation.)

instance Show (SNat n) where
  showsPrec p n@SNat
    = showParen (p > appPrec)
      ( showString "SNat @"
        . showsPrec appPrec1 (TN.natVal n)
      )

instance Show (SSymbol s) where
  showsPrec p s@SSym
    = showParen (p > appPrec)
      ( showString "SSym @"
        . showsPrec appPrec1 (symbolVal s)
      )

------------------------------------------------------------
-- Template Haskell-generated instances
------------------------------------------------------------

$(showSingInstances basicTypes)
