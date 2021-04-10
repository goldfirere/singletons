module T492 where

import Data.Functor.Const
import Data.Functor.Const.Singletons
import Data.Proxy
import Data.Proxy.Singletons
import Prelude.Singletons hiding (Const, ConstSym0, ConstSym1)

f :: Const Bool Ordering
f = Const @Bool @Ordering True

sF1 :: SConst ('Const @Bool @Ordering True)
sF1 = SConst @Bool @Ordering STrue

sF2 :: SConst (ConstSym0 @Bool @Ordering @@ True)
sF2 = singFun1 @(ConstSym0 @Bool @Ordering) SConst @@ STrue

sF3 :: SConst (ConstSym1 @Bool @Ordering True)
sF3 = SConst @Bool @Ordering STrue

sP1 :: SProxy ('Proxy @Bool)
sP1 = SProxy @Bool

sP2 :: SProxy (ProxySym0 @Bool)
sP2 = SProxy @Bool
