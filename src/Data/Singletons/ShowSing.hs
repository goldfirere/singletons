{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
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

import Data.Kind
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
-- 'ShowSing' type synonym as a convenient shorthand. Thus, the above instance
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
type ShowSing k = (forall z. Show (Sing (z :: k)) :: Constraint)

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
