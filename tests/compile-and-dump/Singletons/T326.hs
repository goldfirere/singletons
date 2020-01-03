module T326 where

import Data.Singletons.TH
import Data.Type.Equality

class C1 a where
  infixl 9 <%>
  (<%>) :: a -> a -> a

class C2 a where
  infixl 9 <%%>
  (<%%>) :: a -> a -> a

$(genPromotions [''C1])
$(genSingletons [''C2])

test1 :: Proxy f -> Proxy g -> Proxy h
      -> (f <%> g) <%> h :~: f <%> g <%> h
test1 _ _ _ = Refl

test2 :: Proxy f -> Proxy g -> Proxy h
      -> (f <%%> g) <%%> h :~: f <%%> g <%%> h
test2 _ _ _ = Refl
