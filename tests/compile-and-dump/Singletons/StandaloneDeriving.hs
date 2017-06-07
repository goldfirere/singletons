module Singletons.StandaloneDeriving where

import Data.Singletons.Prelude
import Data.Singletons.Prelude.Show
import Data.Singletons.TH

$(singletons [d|

  infixl 6 :*:
  data T a = a :*: a
  data S = S1 | S2 deriving Eq
  |])

-- We need to put the standalone deriving declarations separately from the
-- data types for the time being due to #192
$(singletons [d|

  deriving instance Show a => Show (T a)

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
