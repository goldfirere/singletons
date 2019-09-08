module T378a where

import Data.Kind
import Data.Singletons.Prelude hiding (Proxy(..))
import Data.Singletons.TH hiding (Proxy(..))

$(singletons [d|
  constBA :: forall b a. a -> b -> a
  constBA x _ = x

  data Proxy :: forall k. k -> Type where
    Proxy1 :: Proxy a
    Proxy2 :: Proxy (a :: k)
    Proxy3 :: forall a. Proxy a
    Proxy4 :: forall k (a :: k). Proxy a
  |])

ex1 :: [Bool]
ex1 = [] @Bool

type PEx1 :: [Bool]
type PEx1 = '[] @Bool

sEx1 :: SList ('[] @Bool)
sEx1 = SNil @Bool

ex2 :: Bool
ex2 = constBA @Ordering @Bool True LT

type PEx2 :: Bool
type PEx2 = ConstBA @Ordering @Bool True LT

sEx2 :: Sing (ConstBA True LT)
sEx2 = sConstBA @Ordering @Bool STrue SLT

proxyEx1, proxyEx2, proxyEx3, proxyEx4 :: Proxy True
proxyEx1 = Proxy1 @True
proxyEx2 = Proxy2 @Bool @True
proxyEx3 = Proxy3 @True
proxyEx4 = Proxy4 @Bool @True

type ProxyEx1 :: Proxy True
type ProxyEx1 = Proxy1 @True

type ProxyEx2 :: Proxy True
type ProxyEx2 = Proxy2 @Bool @True

type ProxyEx3 :: Proxy True
type ProxyEx3 = Proxy3 @True

type ProxyEx4 :: Proxy True
type ProxyEx4 = Proxy4 @Bool @True

sProxyEx1 :: SProxy (Proxy1 @True)
sProxyEx1 = SProxy1 @True

sProxyEx2 :: SProxy (Proxy2 @Bool @True)
sProxyEx2 = SProxy2 @Bool @True

sProxyEx3 :: SProxy (Proxy3 @True)
sProxyEx3 = SProxy3 @True

sProxyEx4 :: SProxy (Proxy4 @Bool @True)
sProxyEx4 = SProxy4 @Bool @True
