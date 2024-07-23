{-# LANGUAGE DeriveAnyClass #-}

module T209 where

import Data.Singletons.TH

$(singletons
  [d| class C a b where
      m :: a -> b -> Bool -> Bool
      m _ _ x = x

      data Hm = Hm
        deriving anyclass (C Bool)

      deriving anyclass instance C a a => C a (Maybe a)
    |])
