{-# LANGUAGE CPP, TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             TemplateHaskell, GADTs, UndecidableInstances, RankNTypes,
             ScopedTypeVariables, FlexibleContexts #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Monad
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Monad (
  (:>>=), (:>>), Return,

  -- * Defunctionalization symbols
  (:>>=$), (:>>=$$), (:>>=$$$),
  (:>>$), (:>>$$), (:>>$$$),
  ReturnSym0, ReturnSym1
 ) where

import Data.Singletons

type family (:>>=) (a :: k1) (b :: TyFun k2 k3 -> *) :: k3
type (:>>=$$$) a b = a :>>= b
data (:>>=$$) a b where
    (:>>=$$###) :: Apply ((:>>=$$) a) arg ~ (:>>=$$$) a arg
                => Proxy arg
                -> (:>>=$$) a b
type instance Apply ((:>>=$$) a) b = (:>>=$$$) a b
data (:>>=$) a where
    (:>>=$###) :: Apply (:>>=$) arg ~ (:>>=$$) arg
                => Proxy arg
               -> (:>>=$) a
type instance Apply (:>>=$) a = (:>>=$$) a

type family (:>>) (a :: k1) (b :: k2) :: k2
type (:>>$$$) a b = a :>> b
data (:>>$$) a b where
    (:>>$$###) :: Apply ((:>>$$) a) arg ~ (:>>$$$) a arg
               => Proxy arg
               -> (:>>$$) a b
type instance Apply ((:>>$$) a) b = (:>>$$$) a b
data (:>>$) a where
    (:>>$###) :: Apply (:>>$) arg ~ (:>>$$) arg
               => Proxy arg
               -> (:>>$) a
type instance Apply (:>>$) a = (:>>$$) a

type family Return (a :: k1) :: k2
type ReturnSym1 a = Return a
data ReturnSym0 a where
    ReturnSym0KindInference :: Apply ReturnSym0 arg ~ ReturnSym1 arg
                            => Proxy arg
                            -> ReturnSym0 a
type instance Apply ReturnSym0 a = ReturnSym1 a

{-
class  Monad m  where
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b
    (>>)        :: forall a b. m a -> m b -> m b

    return      :: a -> m a
    fail        :: String -> m a

    m >> k      = m >>= \_ -> k
    fail s      = error s
-}