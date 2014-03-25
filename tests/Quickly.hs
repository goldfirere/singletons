{- This file simply imports all test case files -- it is a quick check
   that the TH code singletons produces compiles. Run `make` in the
   `tests` directory to compile this file. -}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Quickly where

import Promote.NumArgs
import Promote.PatternMatching
import Promote.Constructors
import Promote.Error
import Promote.Lambdas
import Promote.LambdasComprehensive
#if __GLASGOW_HASKELL__ >= 707
import Promote.Newtypes
#endif
import Promote.GenDefunSymbols
import Promote.CaseExpressions
import Promote.LambdaCase
import Promote.LetStatements
import Promote.Sections

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
-- Can't import tuples test due to duplicate instances
--import Singletons.Tuples ()
import Singletons.ReturnFunc
import Singletons.ZipWith

import GradingClient.Database

import InsertionSort.InsertionSortImp
