{-# LANGUAGE TypeApplications #-}
module Main (
    main
 ) where

import Data.Proxy               ( Proxy(..)                                 )
import SingletonsTestSuiteUtils ( compileAndDumpStdTest, compileAndDumpTest
                                , testCompileAndDumpGroup, cabalArgs
                                , RootDir(..)
                             --   , cleanFiles
                                )
import Test.Tasty               ( TestTree, askOption, defaultIngredients
                                , defaultMainWithIngredients
                                , includingOptions, testGroup               )
import Test.Tasty.Options       ( OptionDescription(..)                     )

main :: IO ()
main = do
--    cleanFiles    We really need to parallelize the testsuite.
    defaultMainWithIngredients ings $ askOption $ \(RootDir rootDir) -> tests rootDir
  where
    ings = includingOptions [Option (Proxy @RootDir)] : defaultIngredients

tests :: FilePath -> TestTree
tests rootDir =
 let stdTest   = compileAndDumpStdTest rootDir
     test name = compileAndDumpTest rootDir name $ cabalArgs rootDir
 in testGroup "Testsuite" $ [
    testCompileAndDumpGroup "Singletons"
    [ stdTest "Nat"
    , stdTest "Empty"
    , stdTest "Maybe"
    , stdTest "BoxUnBox"
    , stdTest "Operators"
    , stdTest "HigherOrder"
    , stdTest "Contains"
    , stdTest "AsPattern"
    , stdTest "DataValues"
    , stdTest "EqInstances"
    , stdTest "CaseExpressions"
    , stdTest "Star"
    , stdTest "ReturnFunc"
    , stdTest "Lambdas"
    , stdTest "LambdasComprehensive"
    , stdTest "Error"
    , stdTest "TopLevelPatterns"
    , stdTest "LetStatements"
    , stdTest "LambdaCase"
    , stdTest "Sections"
    , stdTest "PatternMatching"
    , stdTest "Records"
    , stdTest "T29"
    , stdTest "T33"
    , stdTest "T54"
    , stdTest "Classes"
    , stdTest "Classes2"
    , stdTest "FunDeps"
    , stdTest "T78"
    , stdTest "OrdDeriving"
    , stdTest "BoundedDeriving"
    , stdTest "BadBoundedDeriving"
    , stdTest "EnumDeriving"
    , stdTest "BadEnumDeriving"
    , stdTest "Fixity"
    , stdTest "Undef"
    , stdTest "T124"
    , stdTest "T136"
    , stdTest "T136b"
    , stdTest "T153"
    , stdTest "T157"
    , stdTest "T159"
    , stdTest "T167"
    , stdTest "T145"
    , stdTest "PolyKinds"
    , stdTest "PolyKindsApp"
    , stdTest "T163"
    , stdTest "T166"
    , stdTest "T172"
    , stdTest "T175"
    , stdTest "T176"
    , stdTest "T178"
    , stdTest "T187"
    , stdTest "T190"
    , stdTest "ShowDeriving"
    , stdTest "EmptyShowDeriving"
    , stdTest "StandaloneDeriving"
    , stdTest "T197"
    , stdTest "T197b"
    , stdTest "T200"
    , stdTest "T206"
    , stdTest "T209"
    , stdTest "T226"
    , stdTest "T229"
    , stdTest "T249"
    , stdTest "OverloadedStrings"
    , stdTest "T271"
    , stdTest "T287"
    , stdTest "TypeRepTYPE"
    ],
    testCompileAndDumpGroup "Promote"
    [ stdTest "Constructors"
    , stdTest "GenDefunSymbols"
    , stdTest "Newtypes"
    , stdTest "Pragmas"
    , stdTest "Prelude"
    , stdTest "T180"
    ],
    testGroup "Database client"
    [ test "GradingClient/Database"
    , test "GradingClient/Main"
    ],
    testCompileAndDumpGroup "InsertionSort"
    [ stdTest "InsertionSortImp"
    ]
  ]
