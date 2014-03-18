module Promote.TopLevelPatterns where

import Data.Singletons.TH
import Data.Singletons.List
import Prelude hiding (Bool(..))

$(promote [d|
  data Bool = False | True
  data Foo = Bar (Bool -> Bool) (Bool -> Bool)
 |])

$(promote [d|
  otherwise :: Bool
  otherwise = True

  id :: a -> a
  id x = x

  not :: Bool -> Bool
  not True  = False
  not False = True

  f,g :: Bool
  [f,g] = [otherwise, otherwise]

  h,i :: Bool -> Bool
  (h,i) = (not, id)

  j,k :: Bool -> Bool
  (Bar j k) = Bar not id
 |])
