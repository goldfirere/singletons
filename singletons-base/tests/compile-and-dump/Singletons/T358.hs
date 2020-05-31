module T358 where

import Data.Kind
import Data.Singletons.Prelude.TH

$(singletons [d|
  class C1 (f :: k -> Type) where
    method1 :: f a

  instance C1 [] where
    method1 :: [a]
    method1 = []

  class C2 a where
    method2a, method2b :: forall b. b -> a

  -- Test that variables bound by instance head aren't quantified by the
  -- generated InstanceSigs
  instance C2 [a] where
    method2a _ = []

    method2b :: forall b. b -> [a]
    method2b _ = []
  |])
