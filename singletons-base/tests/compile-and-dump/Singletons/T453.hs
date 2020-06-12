module T453 where

import Control.Monad.Trans.Class
import Data.Kind
import Data.Singletons.TH
import Data.Singletons.TH.Options

$(withOptions defaultOptions{genSingKindInsts = False} $
  singletons $ lift [d|
    type T1 :: forall k. k -> Type
    data T1 a

    type T2 :: k -> Type
    data T2 a
  |])
