{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Proxy
-- Copyright   :  (C) 2020 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports promoted and singled versions of the definitions in "Data.Proxy".
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Proxy (
    -- * The 'Proxy' singleton
    Sing, SProxy(..)
  , AsProxyTypeOf, sAsProxyTypeOf

    -- * Defunctionalization symbols
  , ProxySym0
  , AsProxyTypeOfSym0, AsProxyTypeOfSym1, AsProxyTypeOfSym2
  ) where

import Control.Applicative
import Control.Monad
import Data.Kind
import Data.Proxy
import Data.Semigroup (Semigroup(..))
import Data.Singletons.Decide
import Data.Singletons.Internal
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Semigroup.Internal
import Data.Singletons.Prelude.Show
import Data.Singletons.Promote
import Data.Singletons.Single
import Data.Singletons.TypeLits.Internal
import Data.Type.Coercion
import Data.Type.Equality hiding (type (==))

{-
In order to keep the type argument to Proxy poly-kinded, we define the
singleton version of Proxy by hand. This is very much in the spirit of the
code in D.S.Prelude.Const. (See the comments above SConst in that module
for more details on this choice.)
-}
data SProxy :: forall t. Proxy t -> Type where
  SProxy :: forall t. SProxy ('Proxy @t)
type instance Sing = SProxy
instance SingKind (Proxy t) where
  type Demote (Proxy t) = Proxy t
  fromSing SProxy = Proxy
  toSing Proxy = SomeSing SProxy
instance SingI 'Proxy where
  sing = SProxy

$(genDefunSymbols [''Proxy])

instance SDecide (Proxy t) where
  SProxy %~ SProxy = Proved Refl

instance TestEquality SProxy where
  testEquality = decideEquality

instance TestCoercion SProxy where
  testCoercion = decideCoercion

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
