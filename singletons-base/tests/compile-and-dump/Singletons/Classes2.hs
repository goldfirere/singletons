module Singletons.Classes2 where

import Data.Singletons.Prelude.Ord (EQSym0, LTSym0, GTSym0, Sing(..))
import Data.Singletons.Prelude.TH
import Language.Haskell.TH.Desugar
import Prelude hiding (const)
import Singletons.Classes
import Singletons.Nat


$(singletons [d|
  -- tests promotion of class instances when the class was declared
  -- in a different source file than the instance.
  data NatFoo = ZeroFoo | SuccFoo NatFoo

  instance MyOrd NatFoo where
    ZeroFoo `mycompare` ZeroFoo = EQ
    ZeroFoo `mycompare` (SuccFoo _) = LT
    (SuccFoo _) `mycompare` ZeroFoo = GT
    (SuccFoo n) `mycompare` (SuccFoo m) = m `mycompare` n
 |])
