{-# LANGUAGE LambdaCase, GADTs, ScopedTypeVariables,
             TypeApplications, RankNTypes #-}

module Singletons.T153 where

import Data.Singletons
import Data.Singletons.Prelude

foo :: Int
foo = withSomeSing @(Maybe Bool) (Just True) $ \case
  SJust STrue  -> 0
  SJust SFalse -> 1
  SNothing     -> 2
