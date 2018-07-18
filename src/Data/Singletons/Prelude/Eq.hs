{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             RankNTypes, FlexibleContexts, TemplateHaskell,
             UndecidableInstances, GADTs, DefaultSignatures,
             ScopedTypeVariables, TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Eq
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the SEq singleton version of the Eq type class.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Eq (
  PEq(..), SEq(..),
  DefaultEq,

  -- * Defunctionalization symbols
  type (==@#@$), type (==@#@$$), type (==@#@$$$),
  type (/=@#@$), type (/=@#@$$), type (/=@#@$$$),
  DefaultEqSym0, DefaultEqSym1, DefaultEqSym2
  ) where

import Data.Singletons.Internal
import Data.Singletons.Prelude.Bool
import Data.Singletons.Single
import Data.Singletons.Prelude.Instances
import Data.Singletons.Util
import Data.Singletons.Promote
import qualified Data.Type.Equality as DTE ()

-- NB: These must be defined by hand because of the custom handling of the
-- default for (==) to use DefaultEq

-- | The promoted analogue of 'Eq'. If you supply no definition for '(==)',
-- then it defaults to a use of 'DefaultEq'.
class PEq a where
  type (==) (x :: a) (y :: a) :: Bool
  type (/=) (x :: a) (y :: a) :: Bool

  type (x :: a) == (y :: a) = x `DefaultEq` y
  type (x :: a) /= (y :: a) = Not (x == y)

  infix 4 ==
  infix 4 /=

-- | A sensible way to compute Boolean equality for types of any kind. Note
-- that this definition is slightly different from the '(DTE.==)' type family
-- from "Data.Type.Equality" in @base@, as '(DTE.==)' attempts to distinguish
-- applications of type constructors from other types. As a result,
-- @a == a@ does not reduce to 'True' for every @a@, but @'DefaultEq' a a@
-- /does/ reduce to 'True' for every @a@. The latter behavior is more desirable
-- for @singletons@' purposes, so we use it instead of '(DTE.==)'.
type family DefaultEq (a :: k) (b :: k) :: Bool where
  DefaultEq a a = 'True
  DefaultEq a b = 'False

$(genDefunSymbols [''(==), ''(/=), ''DefaultEq])

-- | The singleton analogue of 'Eq'. Unlike the definition for 'Eq', it is required
-- that instances define a body for '(%==)'. You may also supply a body for '(%/=)'.
class SEq k where
  -- | Boolean equality on singletons
  (%==) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a == b)
  infix 4 %==

  -- | Boolean disequality on singletons
  (%/=) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a /= b)
  default (%/=) :: forall (a :: k) (b :: k).
                   ((a /= b) ~ Not (a == b))
                => Sing a -> Sing b -> Sing (a /= b)
  a %/= b = sNot (a %== b)
  infix 4 %/=

$(singEqInstances basicTypes)

instance SEq a => SingI ((==@#@$) :: a ~> a ~> Bool) where
  sing = singFun2 (%==)
instance (SEq a, SingI x) => SingI ((==@#@$$) x :: a ~> Bool) where
  sing = singFun1 (sing @x %==)

instance SEq a => SingI ((/=@#@$) :: a ~> a ~> Bool) where
  sing = singFun2 (%/=)
instance (SEq a, SingI x) => SingI ((/=@#@$$) x :: a ~> Bool) where
  sing = singFun1 (sing @x %/=)
