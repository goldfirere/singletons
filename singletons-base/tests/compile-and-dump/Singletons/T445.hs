module T445 where

import Data.Singletons.TH
import qualified GHC.TypeLits as Lit
import Numeric.Natural (Natural)
import Prelude.Singletons
import Singletons.Nat

type Lit :: Natural -> Nat
type family Lit n where
  Lit 0 = Zero
  Lit n = Succ (Lit (n Lit.- 1))
$(genDefunSymbols [''Lit])

$(promoteOnly [d|
  evenb :: Nat -> Bool
  evenb Zero = True
  evenb (Succ Zero) = False
  evenb (Succ (Succ n)) = evenb n

  filterEvenGt7 :: [Nat] -> [Nat]
  filterEvenGt7 = filter (\x -> evenb x && x > lit 7)
  |])

testFilterEvenGt7 :: FilterEvenGt7 (Map LitSym0 '[5,2,6,19,129]) :~: '[]
testFilterEvenGt7 = Refl
