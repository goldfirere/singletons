{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies, TypeInType,
             RankNTypes, FlexibleContexts, TemplateHaskell,
             UndecidableInstances, GADTs, DefaultSignatures,
             ScopedTypeVariables, TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Eq
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the SEq singleton version of the Eq type class.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Eq (
  PEq(..), SEq(..),
  type (==@#@$), type (==@#@$$), type (==@#@$$$),
  type (/=@#@$), type (/=@#@$$), type (/=@#@$$$)
  ) where

import Data.Singletons.Internal
import Data.Singletons.Prelude.Bool
import Data.Singletons.Single
import Data.Singletons.Prelude.Instances
import Data.Singletons.Util
import Data.Singletons.Promote
import qualified Data.Type.Equality as DTE

-- NB: These must be defined by hand because of the custom handling of the
-- default for (==) to use Data.Type.Equality.==

-- | The promoted analogue of 'Eq'. If you supply no definition for '(==)',
-- then it defaults to a use of '(DTE.==)', from "Data.Type.Equality".
class PEq a where
  type (==) (x :: a) (y :: a) :: Bool
  type (/=) (x :: a) (y :: a) :: Bool

  type (x :: a) == (y :: a) = x DTE.== y
  type (x :: a) /= (y :: a) = Not (x == y)

  infix 4 ==
  infix 4 /=

$(genDefunSymbols [''(==), ''(/=)])

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
  sing = singFun1 (sing @_ @x %==)

instance SEq a => SingI ((/=@#@$) :: a ~> a ~> Bool) where
  sing = singFun2 (%/=)
instance (SEq a, SingI x) => SingI ((/=@#@$$) x :: a ~> Bool) where
  sing = singFun1 (sing @_ @x %/=)
