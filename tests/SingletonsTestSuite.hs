module Main (
    main
 ) where

import Test.Tasty               ( TestTree, defaultMain, testGroup          )
import SingletonsTestSuiteUtils ( compileAndDumpStdTest, compileAndDumpTest
                                , testCompileAndDumpGroup, ghcOpts          )

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
    , compileAndDumpStdTest "Tuples"
    , compileAndDumpStdTest "ReturnFunc"
    ],
    testCompileAndDumpGroup "Promote"
    [ compileAndDumpStdTest "PatternMatching"
    , compileAndDumpStdTest "Error"
    , compileAndDumpStdTest "Constructors"
    , compileAndDumpStdTest "Lambdas"
    , compileAndDumpStdTest "LambdasComprehensive"
    , compileAndDumpStdTest "GenDefunSymbols"
    , compileAndDumpStdTest "Newtypes"
    , compileAndDumpStdTest "CaseExpressions"
    , compileAndDumpStdTest "LambdaCase"
    , compileAndDumpStdTest "TopLevelPatterns"
    , compileAndDumpStdTest "LetStatements"
    , compileAndDumpStdTest "Sections"
    , compileAndDumpStdTest "Records"
    ],
    testGroup "Database client"
    [ compileAndDumpTest "GradingClient/Database" ghcOpts
    , compileAndDumpTest "GradingClient/Main"     ghcOpts
    ],
    testCompileAndDumpGroup "InsertionSort"
    [ compileAndDumpStdTest "InsertionSortImp"
    ]
  ]
