module Singletons.Fixity where

import Data.Singletons
import Data.Singletons.TH
import Language.Haskell.TH.Desugar
import Prelude.Singletons

$(singletons [d|
  class MyOrd a where
    (<=>) :: a -> a -> Ordering
    infix 4 <=>

  (====) :: a -> a -> a
  a ==== _ = a
  infix 4 ====
 |])
