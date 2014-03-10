{- This file simply imports all test case files -- it is a quick check
   that the TH code singletons produces compiles. Run `make` in the
   `tests` directory to compile this file. -}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Quickly where

import Promote.NumArgs
import Promote.PatternMatching

import Singletons.AtPattern
import Singletons.BoxUnBox
import Singletons.Contains
import Singletons.DataValues
import Singletons.Empty
import Singletons.EqInstances
import Singletons.HigherOrder
import Singletons.Maybe
import Singletons.Nat
import Singletons.Operators
import Singletons.Star

import GradingClient.Database

import InsertionSort.InsertionSortImp