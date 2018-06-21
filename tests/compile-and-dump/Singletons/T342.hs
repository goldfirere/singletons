module T342 where

import Data.Singletons.TH
import Language.Haskell.TH
import Language.Haskell.TH.Desugar

$(do synName <- newName "MyId"
     a       <- newName "a"
     let syn = TySynD synName [PlainTV a] (VarT a)
     defuns <- withLocalDeclarations [syn] $
               genDefunSymbols [synName]
     pure $ syn:defuns)
