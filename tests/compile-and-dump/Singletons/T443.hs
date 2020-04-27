module T443 where

import Control.Monad.Trans.Class
import Data.Kind
import Data.Singletons.TH
import Data.Singletons.TH.Options

$(withOptions defaultOptions{genSingKindInsts = False} $
  singletons $ lift [d|
  data Nat = Z | S Nat

  data Vec :: Nat -> Type -> Type where
    VNil :: Vec Z a
    (:>) :: { head :: a, tail :: Vec n a } -> Vec (S n) a
  |])
