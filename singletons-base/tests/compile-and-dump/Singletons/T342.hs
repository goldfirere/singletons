module T342 where

import Data.Singletons.TH
import Language.Haskell.TH
import Language.Haskell.TH.Desugar

$(do synName <- newName "MyId"
     a       <- newName "a"
     let dsyn = DTySynD synName [DPlainTV a ()] (DVarT a)
         syn  = decToTH dsyn
     defuns <- withLocalDeclarations [syn] $
               genDefunSymbols [synName]
     pure $ syn:defuns)
