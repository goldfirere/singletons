module T204 where

import Control.Monad.Trans.Class
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Language.Haskell.TH

$(let sing_data_con_name :: Name -> Name
      sing_data_con_name n =
        case nameBase n of
          ':':'%':rest -> mkName $ ":^%" ++ rest
          _            -> singledDataConName defaultOptions n in
  withOptions defaultOptions{singledDataConName = sing_data_con_name} $
    singletons $ lift
    [d| data Ratio1 a = a :%  a
        data Ratio2 a = a :%% a
      |])
