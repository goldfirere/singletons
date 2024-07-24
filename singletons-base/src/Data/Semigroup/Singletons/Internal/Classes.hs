{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Singletons.Internal.Classes
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of 'Semigroup', 'PSemigroup'; the
-- singleton version, 'SSemigroup'; and instances thereof for various data
-- types in @base@. These are reexported from the "Data.Semigroup" module or
-- imported directly by some other modules.
--
-- This module exists to avoid import cycles with
-- "Data.Ord.Singletons".
--
----------------------------------------------------------------------------

module Data.Semigroup.Singletons.Internal.Classes where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Singletons.Base.Instances
import Data.Singletons.TH
import GHC.Base.Singletons

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
          go :: a -> [a] -> a
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
  |])
