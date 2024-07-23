{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Fail.Singletons
-- Copyright   :  (C) 2019 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the 'MonadFail' type class.
--
----------------------------------------------------------------------------

module Control.Monad.Fail.Singletons (
  PMonadFail(..), SMonadFail(..),

  -- * Defunctionalization symbols
  FailSym0, FailSym1
  ) where

import Control.Monad.Singletons.Internal
import Data.Kind
import Data.Singletons.Base.Instances
import Data.Singletons.TH

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

  -- See Note [Using standalone kind signatures not present in the base library]
  -- in Control.Monad.Singletons.Internal.
  type MonadFail :: (Type -> Type) -> Constraint
  class Monad m => MonadFail m where
      fail :: String -> m a

  instance MonadFail Maybe where
      fail _ = Nothing

  instance MonadFail [] where
      fail _ = []
  |])
