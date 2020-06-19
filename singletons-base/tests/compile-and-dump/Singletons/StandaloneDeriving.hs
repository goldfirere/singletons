module Singletons.StandaloneDeriving where

import Data.Singletons.Base.TH
import Prelude.Singletons
import Text.Show.Singletons

$(singletons [d|

  infixl 6 :*:
  data T a b = a :*: b
  data S = S1 | S2

  deriving instance Eq a => Eq (T a ())
  deriving instance Ord a => Ord (T a ())
  deriving instance Show a => Show (T a ())

  deriving instance Eq S
  deriving instance Ord S
  deriving instance Show S
  deriving instance Bounded S
  deriving instance Enum S

  |])

-- Ensure that the fixity is discovered
test1 :: "() :*: ()" :~: ShowsPrec 6 ('() :*: '()) ""
test1 = Refl

test2 :: "(() :*: ())" :~: ShowsPrec 7 ('() :*: '()) ""
test2 = Refl
