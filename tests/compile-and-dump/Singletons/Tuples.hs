module Singletons.Tuples where

import Data.Singletons
import Data.Singletons.Single
import Data.Singletons.SuppressUnusedWarnings

$(genSingletons [ ''()
                , ''(,)
                , ''(,,)
                , ''(,,,)
                , ''(,,,,)
                , ''(,,,,,)
                , ''(,,,,,,)
                ])
