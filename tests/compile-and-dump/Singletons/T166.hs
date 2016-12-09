{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module SingletonsBug where

import Data.Singletons.TH
import GHC.TypeLits

$(singletonsOnly [d|
  class Foo a where
    foosPrec :: Nat -> a -> [Bool] -> [Bool]
    foo      :: a -> [Bool]

    foo        x s = foosPrec 0 x s
  |])
