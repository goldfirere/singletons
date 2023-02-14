{-# LANGUAGE OverloadedStrings #-}
module T89 where

import Data.Singletons.Base.TH

$(singletons [d|data Foo = Foo deriving (Enum)|])
