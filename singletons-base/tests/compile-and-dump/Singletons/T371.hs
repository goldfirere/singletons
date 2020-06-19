module T371 where

import Data.Kind
import Data.Singletons.Base.TH

$(singletons [d|
  data X (a :: Type) = X1 | X2 (Y a) deriving Show
  data Y (a :: Type) = Y1 | Y2 (X a) deriving Show
  |])

main :: IO ()
main = do
  print (sing :: Sing ('[] :: [Bool]))
  print (sing :: Sing '[True])
  print (sing :: Sing (X1 :: X Bool))
  print (sing :: Sing (Y2 X1 :: Y Bool))
