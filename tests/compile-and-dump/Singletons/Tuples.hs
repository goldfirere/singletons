module Singletons.Tuples where

import Data.Singletons.Singletons
import Data.Singletons

$(genSingletons [ ''()
                , ''(,)
                , ''(,,)
                , ''(,,,)
                , ''(,,,,)
                , ''(,,,,,)
                , ''(,,,,,,)
                ])
