module T555 where

-- NB: /not/ importing Data.Singletons.Base.TH below, but rather the more
-- restricted Data.Singletons.TH that does not re-export anything from
-- singletons-base. We want to ensure that this code works with only a simple
-- Prelude.Singletons import if possible.
import Data.Singletons.TH
import Prelude.Singletons

$(singletons [d|
  data MyPropKind = Location
                  | Quaternion

      deriving(Eq,Ord,Show)
  |])
