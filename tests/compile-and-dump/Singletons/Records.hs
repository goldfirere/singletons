module Singletons.Records where

import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  data Record a = MkRecord { field1 :: a
                           , field2 :: Bool }

  |])

-- This fails - see #66
-- $(singletons [d|
--  neg :: Record a -> Record a
--  neg rec@(MkRecord { field1 = _, field2 = b } ) = rec {field2 = not b}
-- |])

foo1a :: Proxy (Field2 (MkRecord 5 True))
foo1a = Proxy

foo1b :: Proxy True
foo1b = foo1a

foo2a :: Proxy (Field1 (MkRecord 5 True))
foo2a = Proxy

foo2b :: Proxy 5
foo2b = foo2a
