{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug where

import Data.Singletons.TH

$(singletons [d| data T = T deriving (Eq, Ord, Enum, Bounded, Show) |])
$(singletons [d| data S a = S a deriving (Eq, Ord, Bounded, Show) |])
$(singletons [d| data F a = F { f1 :: a, f2 :: a }
                          | a :***: a
                          deriving (Eq, Ord, Show)
              |])
$(singletons [d| data Void
                 deriving instance Eq Void
                 deriving instance Ord Void
               |])
