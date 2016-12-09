{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module T173 where

import Data.List.NonEmpty
import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  class Foo a where
    bar :: a -> Bool

  instance Foo a => Foo [a] where
    bar []     = True
    bar (x:xs) = bar x && bar xs

  instance Foo a => Foo (NonEmpty a) where
    bar ~(x :| xs) = bar x && bar xs
  |])
