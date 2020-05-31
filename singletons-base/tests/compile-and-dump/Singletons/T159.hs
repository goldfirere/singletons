module T159 where

import Data.Singletons.TH

data T0 = A | B | C | D | E | F
  deriving (Show)

data T1 = N1 | C1 T0 T1 | T0 :&& T1
  deriving (Show)

infixr 5 `C1`
infixr 5 :&&

genSingletons [''T0, ''T1]

singletons [d|
  data T2 = N2 | C2 T0 T2 | T0 :|| T2

  infixr 5 `C2`
  infixr 5 :||
  |]

t1 :: T1
t1 = fromSing $ SA `SC1` SB `SC1` SD :%&& SE :%&& SF `SC1` SN1

t2 :: T2
t2 = fromSing $ SA `SC2` SB `SC2` SD :%|| SE :%|| SF `SC2` SN2
