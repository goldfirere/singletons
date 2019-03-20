{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Monad.Fail
-- Copyright   :  (C) 2019 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the 'MonadFail' type class.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Monad.Fail (
  PMonadFail(..), SMonadFail(..),

  -- * Defunctionalization symbols
  FailSym0, FailSym1
  ) where

import Data.Kind
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Single

$(singletonsOnly [d|
  -- -| When a value is bound in @do@-notation, the pattern on the left
  -- hand side of @<-@ might not match. In this case, this class
  -- provides a function to recover.
  --
  -- A 'Monad' without a 'MonadFail' instance may only be used in conjunction
  -- with pattern that always match, such as newtypes, tuples, data types with
  -- only a single data constructor, and irrefutable patterns (@~pat@).
  --
  -- Instances of 'MonadFail' should satisfy the following law: @fail s@ should
  -- be a left zero for 'Control.Monad.>>=',
  --
  -- @
  -- fail s >>= f  =  fail s
  -- @
  --
  -- If your 'Monad' is also 'Control.Monad.MonadPlus', a popular definition is
  --
  -- @
  -- fail _ = mzero
  -- @
  class Monad m => MonadFail (m :: Type -> Type) where
      fail :: String -> m a

  instance MonadFail Maybe where
      fail _ = Nothing

  instance MonadFail [] where
      fail _ = []
  |])
