module T489 where

import Data.Singletons.Base.TH
import Prelude.Singletons

$(singletons [d|
  blah :: Maybe a -> [a]
  blah (Just @a x)  = [x :: a]
  blah (Nothing @a) = [] :: [a]

  flurmp :: Maybe () -> ()
  flurmp (Nothing @_) = ()
  flurmp (Just ())    = ()
  |])
