{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module T175 where

import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  class Foo a where
    baz :: a

  class Foo a => Bar1 a where
    quux1 :: a
    quux1 = baz

  class Foo a => Bar2 a where

  quux2 :: Bar2 a => a
  quux2 = baz
  |])
