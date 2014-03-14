module Singletons.AtPattern where

import Data.Singletons.TH
import Data.Singletons.Maybe
import Data.Singletons.List
import Singletons.Nat

$(singletons [d|
  maybePlus :: Maybe Nat -> Maybe Nat
  maybePlus (Just n) = Just (plus (Succ Zero) n)
  maybePlus foo@Nothing = foo

{-
  bar :: Maybe Nat -> Maybe Nat
  bar x@(Just y) = x
  bar Nothing = Nothing

  foo :: [Nat] -> [Nat]
  foo f@[]     = f
  foo g@[x]    = g
  foo b@(x:xs) = b

  data Baz = Baz Nat Nat Nat

  baz_ :: Maybe Baz -> Maybe Baz
  baz_ p@Nothing           = p
  baz_ p@(Just (Baz a b c)) = p
-}
 |])
