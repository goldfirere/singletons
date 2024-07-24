{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Eq.Singletons
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of 'Eq', 'PEq', and the singleton version,
-- 'SEq'. Also defines the 'DefaultEq' type family, which is useful for
-- implementing boolean equality for non-inductively defined data types.
--
-----------------------------------------------------------------------------

module Data.Eq.Singletons (
  PEq(..), SEq(..),
  DefaultEq,

  -- * Defunctionalization symbols
  type (==@#@$), type (==@#@$$), type (==@#@$$$),
  type (/=@#@$), type (/=@#@$$), type (/=@#@$$$),
  DefaultEqSym0, DefaultEqSym1, DefaultEqSym2
  ) where

import Data.Bool.Singletons
import Data.Singletons.Base.Instances
import Data.Singletons.Base.Util
import Data.Singletons.TH
-- The imports below are only needed for Haddock purposes.
import qualified Data.Kind as Kind ()
import qualified Data.Type.Equality as DTE ()
import qualified GHC.TypeLits as Lit ()

$(singletonsOnly [d|
  infix 4 ==, /=

  -- -| The 'Eq' class defines equality ('==') and inequality ('/=').
  -- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
  -- and 'Eq' may be derived for any datatype whose constituents are also
  -- instances of 'Eq'.
  --
  -- The Haskell Report defines no laws for 'Eq'. However, '==' is customarily
  -- expected to implement an equivalence relationship where two values comparing
  -- equal are indistinguishable by "public" functions, with a "public" function
  -- being one not allowing to see implementation details. For example, for a
  -- type representing non-normalised natural numbers modulo 100, a "public"
  -- function doesn't make the difference between 1 and 201. It is expected to
  -- have the following properties:
  --
  -- [__Reflexivity__]: @x == x@ = 'True'
  -- [__Symmetry__]: @x == y@ = @y == x@
  -- [__Transitivity__]: if @x == y && y == z@ = 'True', then @x == z@ = 'True'
  -- [__Substitutivity__]: if @x == y@ = 'True' and @f@ is a "public" function
  -- whose return type is an instance of 'Eq', then @f x == f y@ = 'True'
  -- [__Negation__]: @x /= y@ = @not (x == y)@
  --
  -- Minimal complete definition: either '==' or '/='.
  --
  class  Eq a  where
      (==), (/=)           :: a -> a -> Bool

      {-# INLINE (/=) #-}
      {-# INLINE (==) #-}
      x /= y               = not (x == y)
      x == y               = not (x /= y)
      -- {-# MINIMAL (==) | (/=) #-}
  |])

-- | One way to compute Boolean equality for types of any kind. This will
-- return 'True' if the two arguments are known to be the same type and 'False'
-- if they are known to be apart. Examples:
--
-- @
-- >>> 'DefaultEq' 'Nothing' 'Nothing'
-- 'True'
-- >>> 'DefaultEq' 'Nothing' ('Just' a)
-- 'False'
-- >>> 'DefaultEq' a a
-- 'True'
-- @
--
-- 'DefaultEq' is most suited for data types that are not inductively defined.
-- Four concrete examples of this are 'Natural', 'Lit.Symbol', 'Lit.Char', and
-- 'Kind.Type'. One cannot implement boolean equality for these types by
-- pattern matching alone, so 'DefaultEq' is a good fit instead.
--
-- The downside to 'DefaultEq' is that it can fail to reduce if it is unable
-- to determine if two types are equal or apart. Here is one such example:
--
-- @
-- 'DefaultEq' ('Just' a) ('Just' b)
-- @
--
-- What should this reduce to? It depends on what @a@ and @b@ are. 'DefaultEq'
-- has no way of knowing what these two types are, and as a result, this type
-- will be stuck. This is a pitfall that you can run into if you use
-- 'DefaultEq' to implement boolean equality for an inductive data type like
-- 'Maybe'. For this reason, it is usually recommended to implement boolean
-- equality for inductive data types using pattern matching and recursion, not
-- 'DefaultEq'.
--
-- Note that this definition is slightly different from the '(DTE.==)' type
-- family from "Data.Type.Equality" in @base@, as '(DTE.==)' attempts to
-- distinguish applications of type constructors from other types. As a result,
-- @a == a@ does not reduce to 'True' for every @a@, but @'DefaultEq' a a@
-- /does/ reduce to 'True' for every @a@. The latter behavior is more desirable
-- for @singletons@' purposes, so we use it instead of '(DTE.==)'.
type DefaultEq :: k -> k -> Bool
type family DefaultEq (a :: k) (b :: k) :: Bool where
  DefaultEq a a = 'True
  DefaultEq a b = 'False
$(genDefunSymbols [''DefaultEq])

$(singEqInstances basicTypes)
