module T433 where

import Data.Kind (Type)
import Data.Singletons.Base.TH
import Prelude.Singletons

$(promote [d|
  konst1 :: a -> Bool -> a
  konst1 x _ = x

  konst2 :: a -> Maybe Bool -> a
  konst2 x _ = x

  f local = g
    where
      g :: forall a. a -> a
      g x = konst1 (x :: a) local

  foo :: forall a. a -> ()
  foo x = const () (Nothing :: Maybe a)

  id2 :: forall a. a -> a
  id2 x = id (x :: a)

  id3 :: a -> a
  id3 (x :: b) = id (x :: b)

  id4 :: forall a. a -> a
  id4 (x :: a) = id (x :: a)

  id5 :: forall a. a -> a
  id5 = g
    where
      g = id :: a -> a

  id6 :: a -> a
  id6 (x :: b) = g
    where
      g = (x :: b)

  id7 (x :: b) = g
    where
      g = (x :: b)

  id8 :: a -> a
  id8 x = g x True
    where
      g :: b -> a -> b
      g y _ = y

  -- These version will not work. See the singletons README for more about these
  -- limitations.

  -- This will not work because the `a` in `forall a` scopes over the body of
  -- the function, but the `(x :: b)` pattern refers to `b` instead of `a`.
  {-
  id9 :: forall a. a -> a
  id9 (x :: b) = id (x :: b)
  -}

  -- This will not work because the `b` in `Nothing :: Maybe b` is bound by the
  -- type signature for `g`, a local variable that closes over `x`. Moreover,
  -- `b` is only mentioned in the return type (and not the arguments) of the
  -- type signature.
  {-
  id10 :: forall a. a -> a
  id10 x = konst2 x y
    where
      y :: forall b. Maybe b
      y = Nothing :: Maybe b
  -}
  |])
