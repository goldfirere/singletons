{- This file simply imports all test case files -- it is a quick check
   that the TH code singletons produces compiles. Run `make` in the
   `tests` directory to compile this file. -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Quickly where

import Promote.Constructors
#if __GLASGOW_HASKELL__ >= 707
import Promote.Newtypes
#endif
import Promote.GenDefunSymbols
import Promote.Classes
import Promote.TopLevelPatterns

import Singletons.Records
import Singletons.TopLevelPatterns
import Singletons.Sections
import Singletons.PatternMatching
import Singletons.LetStatements
import Singletons.Error
import Singletons.LambdaCase
import Singletons.Lambdas
import Singletons.LambdasComprehensive
import Singletons.CaseExpressions
import Singletons.AsPattern
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

import GradingClient.Database

import InsertionSort.InsertionSortImp
