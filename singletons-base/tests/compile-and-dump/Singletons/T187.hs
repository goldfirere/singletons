module T187 where

import Data.Singletons.Base.TH

$(singletons[d| data Empty
                deriving instance Eq Empty
                deriving instance Ord Empty
              |])
