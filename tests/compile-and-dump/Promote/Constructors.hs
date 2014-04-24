{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Promote.Constructors where

import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.TH

-- Tests defunctionalization symbol generation for :
--  * infix constructors
--  * constructors with arity > 2

$(promote [d|
  data Foo = Foo | Foo :+ Foo
  data Bar = Bar Bar Bar Bar Bar Foo
 |])
