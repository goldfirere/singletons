module Main (
    main
 ) where

import Test.Tasty               ( DependencyType(..), TestTree
                                , after, defaultMain, testGroup )
import SingletonsTestSuiteUtils ( compileAndDumpStdTest, compileAndDumpTest
                                , testCompileAndDumpGroup, ghcOpts )

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
    , afterSingletonsNat .
      compileAndDumpStdTest "Operators"
    , afterSingletonsNat .
      compileAndDumpStdTest "HigherOrder"
    , compileAndDumpStdTest "Contains"
    , afterSingletonsNat .
      compileAndDumpStdTest "AsPattern"
    , afterSingletonsNat .
      compileAndDumpStdTest "DataValues"
    , after AllSucceed "$3 == \"Empty\"" .
      after AllSucceed "$3 == \"Operators\"" .
      compileAndDumpStdTest "EqInstances"
    , compileAndDumpStdTest "CaseExpressions"
    , afterSingletonsNat .
      compileAndDumpStdTest "Star"
    , afterSingletonsNat .
      compileAndDumpStdTest "ReturnFunc"
    , compileAndDumpStdTest "Lambdas"
    , afterSingletonsNat .
      compileAndDumpStdTest "LambdasComprehensive"
    , compileAndDumpStdTest "Error"
    , compileAndDumpStdTest "TopLevelPatterns"
    , afterSingletonsNat .
      compileAndDumpStdTest "LetStatements"
    , compileAndDumpStdTest "LambdaCase"
    , afterSingletonsNat .
      compileAndDumpStdTest "Sections"
    , afterSingletonsNat .
      compileAndDumpStdTest "PatternMatching"
    , compileAndDumpStdTest "Records"
    , compileAndDumpStdTest "T29"
    , compileAndDumpStdTest "T33"
    , compileAndDumpStdTest "T54"
    , afterSingletonsNat .
      compileAndDumpStdTest "Classes"
    , afterSingletonsNat .
      after AllSucceed "$3 == \"Classes\"" .
      compileAndDumpStdTest "Classes2"
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
    , compileAndDumpStdTest "T160"
    , compileAndDumpStdTest "T163"
    , compileAndDumpStdTest "T166"
    , compileAndDumpStdTest "T172"
    , compileAndDumpStdTest "T175"
    , compileAndDumpStdTest "T176"
    , compileAndDumpStdTest "T178"
    , compileAndDumpStdTest "T183"
    , compileAndDumpStdTest "T184"
    , compileAndDumpStdTest "T187"
    , compileAndDumpStdTest "T190"
    , compileAndDumpStdTest "ShowDeriving"
    , compileAndDumpStdTest "EmptyShowDeriving"
    , compileAndDumpStdTest "StandaloneDeriving"
    , compileAndDumpStdTest "T197"
    , compileAndDumpStdTest "T197b"
    , compileAndDumpStdTest "T200"
    , compileAndDumpStdTest "T206"
    , compileAndDumpStdTest "T209"
    , compileAndDumpStdTest "T216"
    , compileAndDumpStdTest "T226"
    , compileAndDumpStdTest "T229"
    , compileAndDumpStdTest "T249"
    , compileAndDumpStdTest "OverloadedStrings"
    , compileAndDumpStdTest "T271"
    , compileAndDumpStdTest "T287"
    , compileAndDumpStdTest "TypeRepTYPE"
    , compileAndDumpStdTest "T297"
    , compileAndDumpStdTest "T312"
    , compileAndDumpStdTest "T313"
    , compileAndDumpStdTest "T316"
    , compileAndDumpStdTest "T322"
    , compileAndDumpStdTest "NatSymbolReflexive"
    , compileAndDumpStdTest "T323"
    , compileAndDumpStdTest "T332"
    , compileAndDumpStdTest "T342"
    , compileAndDumpStdTest "FunctorLikeDeriving"
    , compileAndDumpStdTest "T353"
    , compileAndDumpStdTest "T358"
    , compileAndDumpStdTest "T367"
    , compileAndDumpStdTest "T371"
    , compileAndDumpStdTest "T376"
    , compileAndDumpStdTest "T378a"
    , compileAndDumpStdTest "T401"
    , compileAndDumpStdTest "T402"
    ],
    testCompileAndDumpGroup "Promote"
    [ compileAndDumpStdTest "Constructors"
    , compileAndDumpStdTest "GenDefunSymbols"
    , afterSingletonsNat .
      compileAndDumpStdTest "Newtypes"
    , compileAndDumpStdTest "Pragmas"
    , compileAndDumpStdTest "Prelude"
    , compileAndDumpStdTest "T180"
    , compileAndDumpStdTest "T361"
    ],
    testGroup "Database client"
    [ compileAndDumpTest "GradingClient/Database" ghcOpts
    , after AllSucceed "$3 == \"Database\"" $
      compileAndDumpTest "GradingClient/Main"     ghcOpts
    ],
    testCompileAndDumpGroup "InsertionSort"
    [ compileAndDumpStdTest "InsertionSortImp"
    ]
  ]

afterSingletonsNat :: TestTree -> TestTree
afterSingletonsNat = after AllSucceed "$3 == \"Nat\""
