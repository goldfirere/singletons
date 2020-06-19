module T157 where

import Prelude.Singletons

foo :: SList '["a", "b", "c"]
foo = sing `SCons` sing `SCons` sing
