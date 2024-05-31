module T582 where

import Data.Singletons.TH

$(singletons [d|
  infixl 4 data `foo`
  foo :: a -> a -> a
  x `foo` _ = x

  infixl 4 type `Bar`
  type Bar :: a -> a -> a
  type x `Bar` y = x

  infixl 4 data %%%
  (%%%) :: a -> a -> a
  x %%% _ = x

  infixl 4 type !!!
  type (!!!) :: a -> a -> a
  type x !!! y = x
  |])
