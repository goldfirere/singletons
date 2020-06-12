module T323 where

import Data.Singletons.Prelude
import Data.Type.Equality

test :: f .@#@$$$ (g .@#@$$$ h) :~: f .@#@$$$ g .@#@$$$ h
test = Refl
