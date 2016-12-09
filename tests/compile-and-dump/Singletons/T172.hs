{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T172 where

import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits

$(singletonsOnly [d|
  ($>) :: Nat -> Nat -> Nat
  ($>) = (+)
  |])
