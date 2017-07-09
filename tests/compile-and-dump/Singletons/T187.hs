module T187 where

import Data.Singletons.TH

$(singletons[d| data Empty
                deriving instance Eq Empty
                deriving instance Ord Empty
              |])
