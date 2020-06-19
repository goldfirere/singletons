module Singletons.AsPattern where

import Data.List.Singletons
import Data.Maybe.Singletons
import Data.Singletons.Base.TH
import Singletons.Nat

$(singletons [d|
  maybePlus :: Maybe Nat -> Maybe Nat
  maybePlus (Just n) = Just (plus (Succ Zero) n)
  maybePlus p@Nothing = p

  bar :: Maybe Nat -> Maybe Nat
  bar x@(Just _) = x
  bar Nothing = Nothing

  data Baz = Baz Nat Nat Nat

  baz_ :: Maybe Baz -> Maybe Baz
  baz_ p@Nothing            = p
  baz_ p@(Just (Baz _ _ _)) = p

  tup :: (Nat, Nat) -> (Nat, Nat)
  tup p@(_, _) = p

  foo :: [Nat] -> [Nat]
  foo p@[]      = p
  foo p@[_]     = p
  foo p@(_:_:_) = p
 |])
