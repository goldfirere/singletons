{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Singletons.T167 where

import Data.Singletons.Prelude.TH
import GHC.TypeLits

type DiffList = [Bool] -> [Bool]

$(singletonsOnly [d|
  class Foo a where
    foosPrec :: Nat -> a -> DiffList
    fooList  :: a -> DiffList
    fooList = undefined

  instance Foo a => Foo [a] where
    foosPrec _ = fooList
  |])
