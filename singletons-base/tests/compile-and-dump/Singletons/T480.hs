module T480 where

import Data.Singletons.TH

$(singletons
  [d| f :: _ -> _
      f x = x
  |])
