module T581 where

import Data.Singletons.Base.TH
import Prelude.Singletons

$(singletons
  [d| class C1 a where
        m1 :: forall b. a -> Maybe (a, b)
        m1 _ = Nothing :: Maybe (a, b)

      instance C1 [a] where
        m1 :: forall b. [a] -> Maybe ([a], b)
        m1 _ = Nothing :: Maybe ([a], b)

      class C2 a where
        m2 :: b -> Maybe a
        m2 _ = Nothing :: Maybe a

      instance C2 [a] where
        m2 :: b -> Maybe [a]
        m2 _ = Nothing :: Maybe [a]

      instance C2 (Maybe a) where
        m2 _ = Nothing :: Maybe (Maybe a)

      class C3 a where
        m3 :: forall b. a -> b -> (a, b)
        m3 x y = (x, y) :: (a, b)

      instance C3 (Maybe a) where
        m3 :: Maybe a -> b -> (Maybe a, b)
        m3 x y = (fmap (\xx -> (xx :: a)) x, y)

      instance C3 [a] where
        m3 x y = (fmap (\xx -> (xx :: a)) x, y)
    |])
