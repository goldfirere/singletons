{-# LANGUAGE FunctionalDependencies #-}

module Singletons.FunDeps where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.TypeLits

$( singletons [d|
  class FD a b | a -> b where
    meth :: a -> a
    l2r  :: a -> b

  instance FD Bool Nat where
    meth = not
    l2r False = 0
    l2r True  = 1

  t1 = meth True
--  t2 = l2r False  -- This fails because no FDs in type families
  |])
