module T323 where

import Data.Type.Equality
import Prelude.Singletons

test :: f .@#@$$$ (g .@#@$$$ h) :~: f .@#@$$$ g .@#@$$$ h
test = Refl
