module T229 where

import Data.Singletons.Prelude.TH

$(singletons [d| ___foo :: Bool -> Bool
                 ___foo _ = True |])
