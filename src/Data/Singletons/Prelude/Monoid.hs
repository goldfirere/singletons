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

  Sing(SDual, sGetDual, SAll, sGetAll, SAny, sGetAny, SSum, sGetSum,
       SProduct, sGetProduct, SFirst, sGetFirst, SLast, sGetLast),
  GetDual, GetAll, GetAny, GetSum, GetProduct, GetFirst, GetLast,

  SDual, SAll, SAny, SSum, SProduct, SFirst, SLast,

  -- ** Defunctionalization symbols
  MemptySym0,
  MappendSym0, MappendSym1, MappendSym2,
  MconcatSym0, MconcatSym1,
  DualSym0, DualSym1, GetDualSym0, GetDualSym1,
  AllSym0, AllSym1, GetAllSym0, GetAllSym1,
  AnySym0, AnySym1, GetAnySym0, GetAnySym1,
  SumSym0, SumSym1, GetSumSym0, GetSumSym1,
  ProductSym0, ProductSym1, GetProductSym0, GetProductSym1,
  FirstSym0, FirstSym1, GetFirstSym0, GetFirstSym1,
  LastSym0, LastSym1, GetLastSym0, GetLastSym1
  ) where

import Data.Monoid (First(..), Last(..))
import Data.Ord (Down(..))
import Data.Semigroup hiding (First(..), Last(..))
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Semigroup.Internal hiding
       (Sing(SFirst, SLast), SFirst, SLast,
        FirstSym0, FirstSym1, FirstSym0KindInference,
        LastSym0,  LastSym1,  LastSym0KindInference,
        GetFirst,  GetFirstSym0, GetFirstSym1, GetFirstSym0KindInference,
        GetLast,   GetLastSym0,  GetLastSym1, GetLastSym0KindInference)
import Data.Singletons.Prelude.Show
import Data.Singletons.Single
import Data.Singletons.Util

import GHC.TypeLits (Symbol)

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
        mempty _ = mempty

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

  instance Monoid Symbol where
    mempty = ""
  |])

$(genSingletons        monoidBasicTypes)
$(showSingInstances    monoidBasicTypes)
$(singEqInstances      monoidBasicTypes)
$(singDecideInstances  monoidBasicTypes)
$(singOrdInstances     monoidBasicTypes)
$(singShowInstances    monoidBasicTypes)

$(singletonsOnly [d|
  instance Monoid a => Monoid (Dual a) where
          mempty = Dual mempty

  instance Monoid All where
          mempty = All True

  instance Monoid Any where
          mempty = Any False

  instance Num a => Monoid (Sum a) where
          mempty = Sum 0

  instance Num a => Monoid (Product a) where
          mempty = Product 1

  -- deriving newtype instance Monoid a => Monoid (Down a)
  instance Monoid a => Monoid (Down a) where
      mempty = Down mempty
      Down a `mappend` Down b = Down (a `mappend` b)

  -- deriving newtype instance Applicative First
  instance Applicative First where
    pure = First . pure
    First f <*> First x = First (f <*> x)

  -- deriving instance Functor First
  instance Functor First where
    fmap f (First x) = First (fmap f x)

  -- deriving newtype instance Monad First
  instance Monad First where
    First a >>= k = First (a >>= \x -> case k x of First y -> y)

  instance Semigroup (First a) where
          First Nothing    <> b = b
          a@(First Just{}) <> _ = a

  instance Monoid (First a) where
          mempty = First Nothing

  -- deriving newtype instance Applicative Last
  instance Applicative Last where
    pure = Last . pure
    Last f <*> Last x = Last (f <*> x)

  -- deriving instance Functor Last
  instance Functor Last where
    fmap f (Last x) = Last (fmap f x)

  -- deriving newtype instance Monad Last
  instance Monad Last where
    Last a >>= k = Last (a >>= \x -> case k x of Last y -> y)

  instance Semigroup (Last a) where
          a <> Last Nothing     = a
          _ <> b@(Last Just {}) = b

  instance Monoid (Last a) where
          mempty = Last Nothing
  |])
