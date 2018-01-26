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

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Monoid
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of 'Monoid', 'PMonoid', and the
-- singleton version, 'SMonoid'.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Monoid (
  PMonoid(..), SMonoid(..),

  -- ** Defunctionalization symbols
  MemptySym0,
  MappendSym0, MappendSym1, MappendSym2,
  MconcatSym0, MconcatSym1
  ) where

import Data.Semigroup (Semigroup(..))
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Semigroup
import Data.Singletons.Single

$(singletonsOnly [d|
  -- -| The class of monoids (types with an associative binary operation that
  -- has an identity).  Instances should satisfy the following laws:
  --
  --  * @x '<>' 'mempty' = x@
  --
  --  * @'mempty' '<>' x = x@
  --
  --  * @x '<>' (y '<>' z) = (x '<>' y) '<>' z@ ('Semigroup' law)
  --
  --  * @'mconcat' = 'foldr' '(<>)' 'mempty'@
  --
  -- The method names refer to the monoid of lists under concatenation,
  -- but there are many other instances.
  --
  -- Some types can be viewed as a monoid in more than one way,
  -- e.g. both addition and multiplication on numbers.
  -- In such cases we often define @newtype@s and make those instances
  -- of 'Monoid', e.g. 'Sum' and 'Product'.
  class Semigroup a => Monoid a where
        -- -| Identity of 'mappend'
        mempty  :: a

        -- -| An associative operation
        --
        -- __NOTE__: This method is redundant and has the default
        -- implementation @'mappend' = '(<>)'@.
        mappend :: a -> a -> a
        mappend = (<>)

        -- -| Fold a list using the monoid.
        --
        -- For most types, the default definition for 'mconcat' will be
        -- used, but the function is included in the class definition so
        -- that an optimized version can be provided for specific types.
        mconcat :: [a] -> a
        mconcat = foldr mappend mempty

  instance Monoid [a] where
        mempty  = []
        -- mconcat xss = [x | xs <- xss, x <- xs]

  instance Monoid b => Monoid (a -> b) where
        mempty = \_ -> mempty

  instance Monoid () where
        -- Should it be strict?
        mempty        = ()
        mconcat _     = ()

  instance (Monoid a, Monoid b) => Monoid (a,b) where
        mempty = (mempty, mempty)

  instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
        mempty = (mempty, mempty, mempty)

  instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
        mempty = (mempty, mempty, mempty, mempty)

  instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
                Monoid (a,b,c,d,e) where
        mempty = (mempty, mempty, mempty, mempty, mempty)

  -- lexicographical ordering
  instance Monoid Ordering where
    mempty             = EQ

  -- -| Lift a semigroup into 'Maybe' forming a 'Monoid' according to
  -- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
  -- turned into a monoid simply by adjoining an element @e@ not in @S@
  -- and defining @e*e = e@ and @e*s = s = s*e@ for all @s âˆˆ S@.\"
  instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing
  |])
