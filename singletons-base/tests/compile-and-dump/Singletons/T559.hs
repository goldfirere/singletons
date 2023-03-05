{-# LANGUAGE TypeData #-}
module T559 where

import Data.Singletons.TH

$(singletons [d| type data T = MkT |])
