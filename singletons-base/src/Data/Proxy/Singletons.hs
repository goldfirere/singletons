{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Proxy.Singletons
-- Copyright   :  (C) 2020 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports promoted and singled versions of the definitions in "Data.Proxy".
--
-----------------------------------------------------------------------------

module Data.Proxy.Singletons (
    -- * The 'Proxy' singleton
    Sing, SProxy(..)
  , AsProxyTypeOf, sAsProxyTypeOf

    -- * Defunctionalization symbols
  , ProxySym0
  , AsProxyTypeOfSym0, AsProxyTypeOfSym1, AsProxyTypeOfSym2
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Singletons.Internal
import Data.Eq.Singletons
import Data.Monoid.Singletons
import Data.Ord.Singletons
import Data.Proxy
import Data.Semigroup (Semigroup(..))
import Data.Semigroup.Singletons.Internal.Classes
import Data.Singletons.Decide
import Data.Singletons.Base.Enum
import Data.Singletons.Base.Instances
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Data.Type.Coercion
import Data.Type.Equality hiding (type (==))
import GHC.Base.Singletons
import GHC.Num.Singletons
import GHC.TypeLits.Singletons.Internal
import Text.Show.Singletons

$(withOptions defaultOptions{genSingKindInsts = False}
    (genSingletons [''Proxy]))

instance SingKind (Proxy t) where
  type Demote (Proxy t) = Proxy t
  fromSing SProxy = Proxy
  toSing Proxy = SomeSing SProxy

instance Eq (SProxy z) where
  _ == _ = True

instance SDecide (Proxy t) where
  SProxy %~ SProxy = Proved Refl

instance TestEquality SProxy where
  testEquality = decideEquality

instance TestCoercion SProxy where
  testCoercion = decideCoercion

instance Ord (SProxy z) where
  compare _ _ = EQ

instance Show (SProxy z) where
  showsPrec _ _ = showString "SProxy"

$(singletonsOnly [d|
  deriving instance Bounded (Proxy s)

  -- It's common to use (undefined :: Proxy t) and (Proxy :: Proxy t)
  -- interchangeably, so all of these instances are hand-written to be
  -- lazy in Proxy arguments.

  instance Eq (Proxy s) where
    _ == _ = True

  instance Ord (Proxy s) where
    compare _ _ = EQ

  instance Show (Proxy s) where
    showsPrec _ _ = showString "Proxy"

  instance Enum (Proxy s) where
      succ _               = errorWithoutStackTrace "Proxy.succ"
      pred _               = errorWithoutStackTrace "Proxy.pred"
      fromEnum _           = 0
      -- toEnum 0             = Proxy
      -- toEnum _             = errorWithoutStackTrace "Proxy.toEnum: 0 expected"
      toEnum n             = if n == 0
                             then Proxy
                             else errorWithoutStackTrace "Proxy.toEnum: 0 expected"
      -- enumFrom _           = [Proxy]
      -- enumFromThen _ _     = [Proxy]
      enumFromThenTo _ _ _ = [Proxy]
      enumFromTo _ _       = [Proxy]

  instance Semigroup (Proxy s) where
      _ <> _ = Proxy
      sconcat _ = Proxy
      -- stimes _ _ = Proxy

  instance Monoid (Proxy s) where
      mempty = Proxy
      mconcat _ = Proxy

  instance Functor Proxy where
      fmap _ _ = Proxy

  instance Applicative Proxy where
      pure _ = Proxy
      _ <*> _ = Proxy

  instance Alternative Proxy where
      empty = Proxy
      _ <|> _ = Proxy

  instance Monad Proxy where
      _ >>= _ = Proxy

  instance MonadPlus Proxy

  -- -| 'asProxyTypeOf' is a type-restricted version of 'const'.
  -- It is usually used as an infix operator, and its typing forces its first
  -- argument (which is usually overloaded) to have the same type as the tag
  -- of the second.
  --
  -- >>> import Data.Word
  -- >>> :type asProxyTypeOf 123 (Proxy :: Proxy Word8)
  -- asProxyTypeOf 123 (Proxy :: Proxy Word8) :: Word8
  --
  -- Note the lower-case @proxy@ in the definition. This allows any type
  -- constructor with just one argument to be passed to the function, for example
  -- we could also write
  --
  -- >>> import Data.Word
  -- >>> :type asProxyTypeOf 123 (Just (undefined :: Word8))
  -- asProxyTypeOf 123 (Just (undefined :: Word8)) :: Word8
  asProxyTypeOf :: a -> proxy a -> a
  asProxyTypeOf = const
  |])
