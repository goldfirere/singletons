module Main (
    main
 ) where

import Test.Tasty               ( TestTree, defaultMain, testGroup          )
import SingletonsTestSuiteUtils ( compileAndDumpStdTest, compileAndDumpTest
                                , runProgramTest, testCompileAndDumpGroup
                                , ghcOpts                                   )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Testsuite" $ [
    testCompileAndDumpGroup "Singletons"
    [ compileAndDumpStdTest "Nat"
    , compileAndDumpStdTest "Empty"
    , compileAndDumpStdTest "Maybe"
    , compileAndDumpStdTest "BoxUnBox"
    , compileAndDumpStdTest "Operators"
    , compileAndDumpStdTest "BadPlus"
    , compileAndDumpStdTest "HigherOrder"
    , compileAndDumpStdTest "Contains"
    , compileAndDumpStdTest "AtPattern"
    , compileAndDumpStdTest "DataValues"
    , compileAndDumpStdTest "EqInstances"
    , compileAndDumpStdTest "Star"
    ],
    testCompileAndDumpGroup "Promote"
    [ compileAndDumpStdTest "PatternMatching"
    , compileAndDumpStdTest "NumArgs"
    , compileAndDumpStdTest "Error"
    , compileAndDumpStdTest "Constructors"
    , compileAndDumpStdTest "Lambdas"
    , compileAndDumpStdTest "LambdasComprehensive"
    ],
    testGroup "Database client"
    [ compileAndDumpTest "GradingClient/Database" ghcOpts
    , compileAndDumpTest "GradingClient/Main"     ghcOpts
    , runProgramTest     "GradingClient/Main"     [] -- see Note [No test dependencies]
    ],
    testCompileAndDumpGroup "InsertionSort"
    [ compileAndDumpStdTest "InsertionSortImp"
    ]
  ]

-- Note [No test dependencies]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We should have a dependency between the Database client tests - if
-- compilation of the program fails then there is no point in trying to run
-- it. This is not possible to implement at the moment because tasty does not
-- support test dependencies. See link below for more detail:
--
-- https://github.com/feuerbach/tasty/issues/48
