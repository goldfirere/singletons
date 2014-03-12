module Singletons.Tuples where

import Data.Singletons.Exports ()
import Data.Singletons.Types   ()
import Data.Singletons.Singletons (genSingletons)

$(genSingletons [ ''()
                , ''(,)
                , ''(,,)
                , ''(,,,)
                , ''(,,,,)
                , ''(,,,,,)
                , ''(,,,,,,)
                ])
