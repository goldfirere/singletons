module T197b where

import Data.Singletons.TH

$(singletons
  [d| data a :*: b = a :*: b

      data Pair a b = MkPair a b
      infixr 9 `Pair`, `MkPair`
    |])
