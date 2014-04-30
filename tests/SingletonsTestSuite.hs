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
    , compileAndDumpStdTest "HigherOrder"
    , compileAndDumpStdTest "Contains"
    , compileAndDumpStdTest "AsPattern"
    , compileAndDumpStdTest "DataValues"
    , compileAndDumpStdTest "EqInstances"
    , compileAndDumpStdTest "CaseExpressions"
    , compileAndDumpStdTest "Star"
    , compileAndDumpStdTest "Tuples"
    , compileAndDumpStdTest "ReturnFunc"
    , compileAndDumpStdTest "Lambdas"
    , compileAndDumpStdTest "LambdasComprehensive"
    , compileAndDumpStdTest "Error"
    , compileAndDumpStdTest "TopLevelPatterns"
    , compileAndDumpStdTest "LetStatements"
    , compileAndDumpStdTest "LambdaCase"
    , compileAndDumpStdTest "Sections"
    , compileAndDumpStdTest "PatternMatching"
    , compileAndDumpStdTest "Records"
    ],
    testCompileAndDumpGroup "Promote"
    [ compileAndDumpStdTest "Constructors"
    , compileAndDumpStdTest "GenDefunSymbols"
    , compileAndDumpStdTest "Newtypes"
    , compileAndDumpStdTest "Classes"
    , compileAndDumpStdTest "TopLevelPatterns"
    , compileAndDumpStdTest "Pragmas"
    ],
    testGroup "Database client"
    [ compileAndDumpTest "GradingClient/Database" ghcOpts
    , compileAndDumpTest "GradingClient/Main"     ghcOpts
    ],
    testCompileAndDumpGroup "InsertionSort"
    [ compileAndDumpStdTest "InsertionSortImp"
    ]
  ]
