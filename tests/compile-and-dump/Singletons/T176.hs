{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module T176 where

import Data.Kind (Type)
import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  class Foo1 a where
    bar1 :: a -> (a -> b) -> b
    baz1 :: a

  quux1 :: Foo1 a => a -> a
  quux1 x = x `bar1` \_ -> baz1

  class Foo2 a where
    bar2 :: a -> b -> b
    baz2 :: a

  quux2 :: Foo2 a => a -> a
  quux2 x = x `bar2` baz2
  |])
