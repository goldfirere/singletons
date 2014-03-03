module Promote.LambdasComprehensive where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(promote [d|
 data Nat = Zero | Succ Nat

 pred :: Nat -> Nat
 pred Zero = Zero
 pred (Succ n) = n

 foo :: [Nat]
 foo = map (\x -> either_ pred Succ x) [Left Zero, Right (Succ Zero)]
 |])
