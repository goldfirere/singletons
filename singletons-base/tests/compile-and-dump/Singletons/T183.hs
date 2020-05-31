module T183 where

import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  -----
  -- Examples from #183
  -----

  f1 (x :: Maybe Bool) = (x :: Maybe Bool)
  f2 (x :: Maybe a) = (x :: Maybe a)
  f3 (Just a :: Maybe Bool) = "hi"

  g x = case Just x of
    (Just y :: Maybe Bool) -> (y :: Bool)

  -----
  -- Using explicit type signatures
  -----

  -- No explicit forall
  foo1 :: Maybe a -> a
  foo1 (Just x :: Maybe a) = (x :: a)

  -- Explicit forall
  foo2, foo3 :: forall a. Maybe a -> a
  foo2 (Just x :: Maybe a) = (x :: a)
  foo3 (Just x) = (x :: a)

  -----
  -- Multiple pattern signatures
  -----

  foo4 :: (a, b) -> (b, a)
  foo4 = \(x :: a, y :: b) -> (y :: b, x :: a)

  foo5, foo6 :: Maybe (Maybe a) -> Maybe (Maybe a)
  foo5 (Just (Just (x :: a) :: Maybe a) :: Maybe (Maybe a))
      = Just (Just (x :: a) :: Maybe a) :: Maybe (Maybe a)
  foo6 (Just x :: Maybe (Maybe a))
      = case x :: Maybe a of
          (Just (y :: a) :: Maybe a) -> Just (Just (y :: a) :: Maybe a)

  -----
  -- Other pattern features
  -----

  foo7 :: a -> b -> a
  foo7 (x :: a) (_ :: b) = (x :: a)

  foo8 :: forall a. Maybe a -> Maybe a
  foo8 x@(Just (_ :: a) :: Maybe a) = x
  foo8 x@(Nothing :: Maybe a)       = x

  -----
  -- Type variable scoping (vis-à-vis #297)
  -----

  foo9 :: a -> a
  foo9 (x :: a)
    = let g :: a -> b -> a
          g y _ = y
      in g x ()
  |])
