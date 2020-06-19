{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T172 where

import Data.Singletons.TH
import GHC.TypeLits.Singletons
import Prelude.Singletons

$(singletonsOnly [d|
  ($>) :: Nat -> Nat -> Nat
  ($>) = (+)
  |])
