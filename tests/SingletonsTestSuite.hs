module Main (
    main
 ) where

import Test.Tasty               ( TestTree, defaultMain, testGroup          )
import SingletonsTestSuiteUtils ( compileAndDumpStdTest, compileAndDumpTest
                                , testCompileAndDumpGroup, ghcOpts
                             --   , cleanFiles
                                )

main :: IO ()
main = do
--  cleanFiles    We really need to parallelize the testsuite.
  defaultMain tests

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
    , compileAndDumpStdTest "T29"
    , compileAndDumpStdTest "T33"
    , compileAndDumpStdTest "T54"
    , compileAndDumpStdTest "Classes"
    , compileAndDumpStdTest "Classes2"
    , compileAndDumpStdTest "FunDeps"
    , compileAndDumpStdTest "T78"
    , compileAndDumpStdTest "OrdDeriving"
    , compileAndDumpStdTest "BoundedDeriving"
    , compileAndDumpStdTest "BadBoundedDeriving"
    , compileAndDumpStdTest "EnumDeriving"
    , compileAndDumpStdTest "BadEnumDeriving"
    , compileAndDumpStdTest "Fixity"
    , compileAndDumpStdTest "Undef"
    , compileAndDumpStdTest "T124"
    , compileAndDumpStdTest "T136"
    , compileAndDumpStdTest "T136b"
    , compileAndDumpStdTest "T153"
    , compileAndDumpStdTest "T157"
    , compileAndDumpStdTest "T159"
    , compileAndDumpStdTest "T167"
    , compileAndDumpStdTest "T145"
    , compileAndDumpStdTest "PolyKinds"
    , compileAndDumpStdTest "PolyKindsApp"
    , compileAndDumpStdTest "T166"
    , compileAndDumpStdTest "T172"
    , compileAndDumpStdTest "T175"
    , compileAndDumpStdTest "T176"
    , compileAndDumpStdTest "T178"
    , compileAndDumpStdTest "T187"
    , compileAndDumpStdTest "ShowDeriving"
    , compileAndDumpStdTest "BadShowDeriving"
    , compileAndDumpStdTest "StandaloneDeriving"
    , compileAndDumpStdTest "T197"
    , compileAndDumpStdTest "T206"
    ],
    testCompileAndDumpGroup "Promote"
    [ compileAndDumpStdTest "Constructors"
    , compileAndDumpStdTest "GenDefunSymbols"
    , compileAndDumpStdTest "Newtypes"
    , compileAndDumpStdTest "Pragmas"
    , compileAndDumpStdTest "Prelude"
    , compileAndDumpStdTest "T180"
    ],
    testGroup "Database client"
    [ compileAndDumpTest "GradingClient/Database" ghcOpts
    , compileAndDumpTest "GradingClient/Main"     ghcOpts
    ],
    testCompileAndDumpGroup "InsertionSort"
    [ compileAndDumpStdTest "InsertionSortImp"
    ]
  ]
