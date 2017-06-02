module T187 where

import Language.Haskell.TH
import Data.Singletons.Prelude
import Data.Singletons.TH

data Empty

-- We need to construct the TH AST for Empty directly for now, as singletons
-- doesn't support standalone deriving declarations
$(singletonsOnly (return [DataD [] ''Empty [] Nothing []
                            [DerivClause Nothing [ConT ''Eq, ConT ''Ord]]]))
