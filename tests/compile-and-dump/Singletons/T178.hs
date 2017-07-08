module T178 where

import GHC.TypeLits
import Data.Singletons.TH

$(singletons [d|

  -- Note: Ord automatically defines "max"
  data Occ = Str | Opt | Many deriving (Eq, Ord, Show)

  type U = [(Symbol,Occ)]

  empty :: U
  empty = []
  |])
