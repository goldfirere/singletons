module T313 where

import Data.Kind
import Data.Singletons.TH

$(promote [d|
  type PFoo1 a = Maybe a

  type family   PFoo2 a
  type instance PFoo2 a = Maybe a

  type family PFoo3 a where
    PFoo3 a = Maybe a

  class PC (a :: Type) where
    type PFoo4 a
    type PFoo4 a = Maybe a

  instance PC a where
    type PFoo4 a = Maybe a
  |])

$(singletons [d|
  type SFoo1 a = Maybe a

  type family   SFoo2 a
  type instance SFoo2 a = Maybe a

  type family SFoo3 a where
    SFoo3 a = Maybe a

  class SC (a :: Type) where
    type SFoo4 a
    type SFoo4 a = Maybe a

  instance SC a where
    type SFoo4 a = Maybe a
  |])
