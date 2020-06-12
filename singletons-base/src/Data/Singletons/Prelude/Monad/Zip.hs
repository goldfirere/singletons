{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Monad.Zip
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the 'MonadZip' type class.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Monad.Zip (
  PMonadZip(..), SMonadZip(..),

  -- * Defunctionalization symbols
  MzipSym0, MzipSym1, MzipSym2,
  MzipWithSym0, MzipWithSym1, MzipWithSym2, MzipWithSym3,
  MunzipSym0, MunzipSym1,
  ) where

import Data.Functor.Identity
import Data.Monoid
import Data.Proxy
import Data.Singletons.Prelude.Identity
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.List
       ( ZipSym0, ZipWithSym0, UnzipSym0
       , sZip,    sZipWith,    sUnzip )
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Monoid ()
import Data.Singletons.Prelude.Proxy
import Data.Singletons.Prelude.Tuple
import Data.Singletons.TH

$(singletonsOnly [d|
  -- -| `MonadZip` type class. Minimal definition: `mzip` or `mzipWith`
  --
  -- Instances should satisfy the laws:
  --
  -- -* Naturality :
  --
  --   > liftM (f *** g) (mzip ma mb) = mzip (liftM f ma) (liftM g mb)
  --
  -- -* Information Preservation:
  --
  --   > liftM (const ()) ma = liftM (const ()) mb
  --   > ==>
  --   > munzip (mzip ma mb) = (ma, mb)
  --
  class Monad m => MonadZip m where
      -- {-# MINIMAL mzip | mzipWith #-}

      mzip :: m a -> m b -> m (a,b)
      mzip = mzipWith (,)

      mzipWith :: (a -> b -> c) -> m a -> m b -> m c
      mzipWith f ma mb = liftM (uncurry f) (mzip ma mb)

      munzip :: m (a,b) -> (m a, m b)
      munzip mab = (liftM fst mab, liftM snd mab)
      -- munzip is a member of the class because sometimes
      -- you can implement it more efficiently than the
      -- above default code.  See Trac #4370 comment by giorgidze

  instance MonadZip [] where
      mzip     = zip
      mzipWith = zipWith
      munzip   = unzip

  instance MonadZip Identity where
      mzipWith = liftM2
      munzip (Identity (a, b)) = (Identity a, Identity b)

  instance MonadZip Dual where
      -- Cannot use coerce, it's unsafe
      mzipWith = liftM2

  instance MonadZip Sum where
      mzipWith = liftM2

  instance MonadZip Product where
      mzipWith = liftM2

  instance MonadZip Maybe where
      mzipWith = liftM2

  instance MonadZip First where
      mzipWith = liftM2

  instance MonadZip Last where
      mzipWith = liftM2

  instance MonadZip Proxy where
      mzipWith _ _ _ = Proxy
  |])
