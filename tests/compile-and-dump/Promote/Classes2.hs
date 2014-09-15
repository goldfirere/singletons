module Promote.Classes2 where

import Prelude hiding (const)
import Singletons.Nat
import Promote.Classes
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude.Ord (EQSym0, LTSym0, GTSym0)
import Language.Haskell.TH.Desugar

-- tests promotion of class instances when the class was declared
-- in a different source file than the instance.
data NatFoo = ZeroFoo | SuccFoo NatFoo

$(promoteOnly [d|
  instance MyOrd NatFoo where
    ZeroFoo `mycompare` ZeroFoo = EQ
    ZeroFoo `mycompare` (SuccFoo _) = LT
    (SuccFoo _) `mycompare` ZeroFoo = GT
    (SuccFoo n) `mycompare` (SuccFoo m) = m `mycompare` n
 |])
