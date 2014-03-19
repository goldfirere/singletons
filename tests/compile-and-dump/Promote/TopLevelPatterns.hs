module Promote.TopLevelPatterns where

import Data.Singletons.TH
import Data.Singletons.List
import Prelude hiding (Bool(..))

$(promote [d|
  data Bool = False | True
  data Foo = Bar Bool Bool
 |])

$(promote [d|
  otherwise :: Bool
  otherwise = True

  id :: a -> a
  id x = x

  not :: Bool -> Bool
  not True  = False
  not False = True

  f,g :: Bool -> Bool
  [f,g] = [not, id]

  h,i :: Bool -> Bool
  (h,i) = (f, g)

  j,k :: Bool
  (Bar j k) = Bar True (h False)

  l,m :: Bool
  [l,m] = [not True, id False]
 |])
