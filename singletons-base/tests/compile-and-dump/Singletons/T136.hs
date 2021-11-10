module Binary where

import Data.Singletons.Base.Enum
import Data.Singletons.TH
import Prelude.Singletons

type Bit = Bool
type BiNat = [Bit]

$(singletons [d|
  instance Enum BiNat where
    succ [] = [True]
    succ (False:as) = True : as
    succ (True:as) = False : succ as

    pred [] = error "pred 0"
    pred (False:as) = True : pred as
    pred (True:as) = False : as

    toEnum i | i < 0 = error "negative toEnum"
             | i == 0 = []
             | otherwise = succ (toEnum (pred i))

    fromEnum [] = 0
    fromEnum (False:as) = 2 * fromEnum as
    fromEnum (True:as) = 1 + 2 * fromEnum as
  |])
