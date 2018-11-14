{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  ShowSing
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
-- deriving instance ??? => Show (Sing (x :: [k]))
-- @
--
-- To figure out what should go in place of @???@, observe that we require the
-- type of each field to also be 'Show' instances. In other words, we need
-- something like @(Show (Sing (a :: k)))@. But this isn't quite right, as the
-- type variable @a@ doesn't appear in the instance head. In fact, this @a@
-- type is really referring to an existentially quantified type variable in the
-- 'SCons' constructor, so it doesn't make sense to try and use it like this.
--
-- Luckily, the @QuantifiedConstraints@ language extension provides a solution
-- to this problem. This lets you write a context of the form
-- @(forall a. Show (Sing (a :: k)))@, which demands that there be an instance
-- for @Show (Sing (a :: k))@ that is parametric in the use of @a@. Thus, our
-- final instance looks like:
--
-- @
-- deriving instance (forall a. Show (Sing (a :: k))) => Show (Sing (x :: [k]))
-- @
--
-- Because that quantified constraint is somewhat lengthy, we provide the
-- 'ShowSing' class synonym as a convenient shorthand. Thus, the above instance
-- is equivalent to:
--
-- @
-- deriving instance ShowSing k => Show (Sing (x :: [k]))
-- @
--
-- When singling a derived 'Show' instance, @singletons@ will also derive
-- a 'Show' instance for the corresponding singleton type using 'ShowSing'.
-- In other words, if you give @singletons@ a derived 'Show' instance, then
-- you'll receive the following in return:
--
-- * A promoted (@PShow@) instance
-- * A singled (@SShow@) instance
-- * A 'Show' instance for the singleton type
--
-- What a bargain!
class    (forall (z :: k). Show (Sing z)) => ShowSing k
instance (forall (z :: k). Show (Sing z)) => ShowSing k

{-
Note [Define ShowSing as a class, not a type synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In an ideal world, we would simply define ShowSing as an ordinary type synonym,
like this:

  type ShowSing k = (forall (z :: k). Show (Sing z) :: Constraint)

In fact, I used to assume that we lived in an ideal world, so I defined
ShowSing as a type synonym in version 2.5 of this library. However, I realized
some time after 2.5's release that the world is far from ideal, unfortunately,
and that this approach is unfeasible at the time being due to GHC Trac #15888.

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

  deriving instance (forall z. Show (Sing (z :: Y a))) => Show (Sing (z :: X a))
  deriving instance (forall z. Show (Sing (z :: X a))) => Show (Sing (z :: Y a))

Due to the way QuantifiedConstraints currently works (as surmised in Trac
#15888), when GHC has a Wanted `Show (Sing X1 :: X Bool)` constraint, it
chooses the appropriate instance and emits a Wanted
`forall z. Show (Sing (z :: Y Bool))` constraint (from the instance context).
GHC skolemizes the `z` to `z1` and tries to solve a Wanted
`Show (Sing (z1 :: Y Bool))` constraint. GHC chooses the appropriate instance
and emits a Wanted `forall z. Show (Sing (z :: X Bool))` constraint. GHC
skolemizes the `z` to `z2` and tries to solve a Wanted
`Show (Sing (z2 :: X Bool))` constraint... we repeat the process and find
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
