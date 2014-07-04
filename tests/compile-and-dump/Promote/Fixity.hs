module Promote.Fixity where

import Data.Singletons
import Data.Promotion.TH
import Data.Promotion.Prelude
import Language.Haskell.TH.Desugar

-- Fixity declarations don't work due to GHC bug #9066, so we drop
-- them and issue a warning
$(promote [d|
  class MyOrd a where
    (<=>) :: a -> a -> Ordering
    infix 4 <=>

  (====) :: a -> a -> a
  a ==== _ = a
  infix 4 ====
 |])
