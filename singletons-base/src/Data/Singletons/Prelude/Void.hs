{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Void
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for 'Void',
-- including singled versions of all the definitions in @Data.Void@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Void@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------
module Data.Singletons.Prelude.Void (
  -- * The 'Void' singleton
  Sing,
  -- | Just as 'Void' has no constructors, the 'Sing' instance above also has
  -- no constructors.

  SVoid,
  -- | 'SVoid' is a kind-restricted synonym for 'Sing':
  -- @type SVoid (a :: Void) = Sing a@

  -- * Singletons from @Data.Void@
  Absurd, sAbsurd,

  -- * Defunctionalization symbols
  AbsurdSym0, AbsurdSym1
  ) where

import Data.Singletons
import Data.Singletons.Prelude.Instances
import Data.Singletons.TH

$(singletonsOnly [d|
  -- -| Since 'Void' values logically don't exist, this witnesses the
  -- logical reasoning tool of \"ex falso quodlibet\".
  absurd :: Void -> a
  absurd a = case a of {}
  |])
