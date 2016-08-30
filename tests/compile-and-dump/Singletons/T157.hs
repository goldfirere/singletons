module T157 where

import Data.Singletons.Prelude

foo :: SList '["a", "b", "c"]
foo = sing `SCons` sing `SCons` sing
