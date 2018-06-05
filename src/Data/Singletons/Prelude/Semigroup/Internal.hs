{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Semigroup.Internal
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of 'Semigroup', 'PSemigroup'; the
-- singleton version, 'SSemigroup'; and some @newtype@ wrappers, all
-- of which are reexported from the "Data.Semigroup" module or
-- imported directly by some other modules.
--
-- This module exists to avoid import cycles with
-- "Data.Singletons.Prelude.Monoid".
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Semigroup.Internal where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord (Down(..))
import Data.Proxy
import Data.Semigroup (Dual(..), All(..), Any(..), Sum(..), Product(..), Option(..))
import Data.Singletons.Internal
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord hiding (MinSym0, MinSym1, MaxSym0, MaxSym1)
import Data.Singletons.Promote
import Data.Singletons.Single
import Data.Singletons.TypeLits.Internal
import Data.Singletons.Util
import qualified Data.Text as T
import Data.Void (Void)

import GHC.TypeLits (AppendSymbol, SomeSymbol(..), someSymbolVal, Symbol)

import Unsafe.Coerce

$(singletonsOnly [d|
  -- -| The class of semigroups (types with an associative binary operation).
  --
  -- Instances should satisfy the associativity law:
  --
  --  * @x '<>' (y '<>' z) = (x '<>' y) '<>' z@
  class Semigroup a where
        -- -| An associative operation.
        (<>) :: a -> a -> a
        infixr 6 <>

        -- -| Reduce a non-empty list with @\<\>@
        --
        -- The default definition should be sufficient, but this can be
        -- overridden for efficiency.
        --
        sconcat :: NonEmpty a -> a
        sconcat (a :| as) = go a as where
          go b (c:cs) = b <> go c cs
          go b []     = b

        {-
        Can't single 'stimes', since there's no singled 'Integral' class.

        -- -| Repeat a value @n@ times.
        --
        -- Given that this works on a 'Semigroup' it is allowed to fail if
        -- you request 0 or fewer repetitions, and the default definition
        -- will do so.
        --
        -- By making this a member of the class, idempotent semigroups
        -- and monoids can upgrade this to execute in /O(1)/ by
        -- picking @stimes = 'stimesIdempotent'@ or @stimes =
        -- 'stimesIdempotentMonoid'@ respectively.
        stimes :: Integral b => b -> a -> a
        stimes = stimesDefault
        -}


  instance Semigroup [a] where
        (<>) = (++)

  instance Semigroup (NonEmpty a) where
        (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

  instance Semigroup b => Semigroup (a -> b) where
        f <> g = \x -> f x <> g x

  instance Semigroup () where
        _ <> _      = ()
        sconcat _   = ()

  instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
        (a,b) <> (a',b') = (a<>a',b<>b')

  instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
        (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')

  instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (a, b, c, d) where
        (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')

  instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e)
         => Semigroup (a, b, c, d, e) where
        (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')

  instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> y = y
    GT <> _ = GT

  instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

  instance Semigroup (Either a b) where
    Left _    <> b = b
    -- a      <> _ = a
    a@Right{} <> _ = a

  instance Semigroup Void where
    a <> _ = a

  -- deriving newtype instance Semigroup a => Semigroup (Down a)
  instance Semigroup a => Semigroup (Down a) where
    Down a <> Down b = Down (a <> b)
  |])

-- Workaround for #326
infixr 6 <>

$(genSingletons       $ ''Option : semigroupBasicTypes)
$(singBoundedInstances             semigroupBasicTypes)
$(singEqInstances     $ ''Option : semigroupBasicTypes)
$(singDecideInstances $ ''Option : semigroupBasicTypes)
$(singOrdInstances    $ ''Option : semigroupBasicTypes)

$(singletonsOnly [d|
  instance Applicative Dual where
    pure = Dual
    Dual f <*> Dual x = Dual (f x)

  -- deriving instance Functor Dual
  instance Functor Dual where
    fmap f (Dual x) = Dual (f x)

  instance Monad Dual where
    Dual a >>= k = k a

  instance Semigroup a => Semigroup (Dual a) where
          Dual a <> Dual b = Dual (b <> a)

  instance Semigroup All where
          All a <> All b = All (a && b)

  instance Semigroup Any where
          Any a <> Any b = Any (a || b)

  instance Applicative Sum where
    pure = Sum
    Sum f <*> Sum x = Sum (f x)

  -- deriving instance Functor Sum
  instance Functor Sum where
    fmap f (Sum x) = Sum (f x)

  instance Monad Sum where
    Sum a >>= k = k a

  instance Num a => Semigroup (Sum a) where
          Sum a <> Sum b = Sum (a + b)

  -- deriving newtype instance Num a => Num (Sum a)
  instance Num a => Num (Sum a) where
      Sum a + Sum b = Sum (a + b)
      Sum a - Sum b = Sum (a - b)
      Sum a * Sum b = Sum (a * b)
      negate (Sum a) = Sum (negate a)
      abs    (Sum a) = Sum (abs a)
      signum (Sum a) = Sum (signum a)
      fromInteger n  = Sum (fromInteger n)

  instance Applicative Product where
    pure = Product
    Product f <*> Product x = Product (f x)

  -- deriving instance Functor Product
  instance Functor Product where
    fmap f (Product x) = Product (f x)

  instance Monad Product where
    Product a >>= k = k a

  instance Num a => Semigroup (Product a) where
          Product a <> Product b = Product (a * b)

  -- deriving newtype instance Num a => Num (Product a)
  instance Num a => Num (Product a) where
      Product a + Product b = Product (a + b)
      Product a - Product b = Product (a - b)
      Product a * Product b = Product (a * b)
      negate (Product a) = Product (negate a)
      abs    (Product a) = Product (abs a)
      signum (Product a) = Product (signum a)
      fromInteger n      = Product (fromInteger n)
  |])

instance PSemigroup Symbol where
  type a <> b = AppendSymbol a b

instance SSemigroup Symbol where
  sa %<> sb =
    let a  = fromSing sa
        b  = fromSing sb
        ex = someSymbolVal $ T.unpack $ a <> b
    in case ex of
         SomeSymbol (_ :: Proxy ab) -> unsafeCoerce (SSym :: Sing ab)

-- We need these in Data.Singletons.Prelude.Semigroup, as we need to promote
-- code that simultaneously uses the Min/Max constructors and the min/max
-- functions, which have clashing defunctionalization symbol names. Our
-- workaround is to simply define synonyms for min/max and use those instead.
min_, max_ :: Ord a => a -> a -> a
min_ = min
max_ = max

type Min_ x y = Min x y
type Max_ x y = Max x y
$(genDefunSymbols [''Min_, ''Max_])

sMin_ :: forall a (x :: a) (y :: a). SOrd a => Sing x -> Sing y -> Sing (x `Min` y)
sMin_ = sMin

sMax_ :: forall a (x :: a) (y :: a). SOrd a => Sing x -> Sing y -> Sing (x `Max` y)
sMax_ = sMax
