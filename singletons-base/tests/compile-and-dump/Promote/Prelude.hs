module Promote.Prelude where

import Data.List.Singletons
import Data.Singletons.TH
import GHC.TypeLits
import Prelude.Singletons

lengthTest1a :: Proxy (Length '[True, True, True, True])
lengthTest1a = Proxy

lengthTest1b :: Proxy 4
lengthTest1b = lengthTest1a

lengthTest2a :: Proxy (Length '[])
lengthTest2a = Proxy

lengthTest2b :: Proxy 0
lengthTest2b = lengthTest2a

sumTest1a :: Proxy (Sum '[1, 2, 3, 4])
sumTest1a = Proxy

sumTest1b :: Proxy 10
sumTest1b = sumTest1a

sumTest2a :: Proxy (Sum '[])
sumTest2a = Proxy

sumTest2b :: Proxy 0
sumTest2b = sumTest2a

productTest1a :: Proxy (Product '[1, 2, 3, 4])
productTest1a = Proxy

productTest1b :: Proxy 24
productTest1b = productTest1a

productTest2a :: Proxy (Product '[])
productTest2a = Proxy

productTest2b :: Proxy 1
productTest2b = productTest2a

takeTest1a :: Proxy (Take 2 '[1, 2, 3, 4])
takeTest1a = Proxy

takeTest1b :: Proxy '[1, 2]
takeTest1b = takeTest1a

takeTest2a :: Proxy (Take 2 '[])
takeTest2a = Proxy

takeTest2b :: Proxy '[]
takeTest2b = takeTest2a

dropTest1a :: Proxy (Drop 2 '[1, 2, 3, 4])
dropTest1a = Proxy

dropTest1b :: Proxy '[3, 4]
dropTest1b = dropTest1a

dropTest2a :: Proxy (Drop 2 '[])
dropTest2a = Proxy

dropTest2b :: Proxy '[]
dropTest2b = dropTest2a

splitAtTest1a :: Proxy (SplitAt 2 '[1, 2, 3, 4])
splitAtTest1a = Proxy

splitAtTest1b :: Proxy ( '( '[1,2], '[3, 4] ) )
splitAtTest1b = splitAtTest1a

splitAtTest2a :: Proxy (SplitAt 2 '[])
splitAtTest2a = splitAtTest2b

splitAtTest2b :: Proxy ( '( '[], '[] ) )
splitAtTest2b = Proxy

indexingTest1a :: Proxy ('[4, 3, 2, 1] !! 1)
indexingTest1a = Proxy

indexingTest1b :: Proxy 3
indexingTest1b = indexingTest1a

indexingTest2a :: Proxy ('[] !! 0)
indexingTest2a = Proxy

indexingTest2b :: Proxy (Error "Data.Singletons.List.!!: index too large")
indexingTest2b = indexingTest2a

replicateTest1a :: Proxy (Replicate 2 True)
replicateTest1a = Proxy

replicateTest1b :: Proxy '[True, True]
replicateTest1b = replicateTest1a

replicateTest2a :: Proxy (Replicate 0 True)
replicateTest2a = replicateTest2b

replicateTest2b :: Proxy '[]
replicateTest2b = Proxy

$(promoteOnly [d|
  odd :: Natural -> Bool
  odd 0 = False
  odd n = not . odd $ n - 1
 |])

findIndexTest1a :: Proxy (FindIndex OddSym0 '[2,4,6,7])
findIndexTest1a = Proxy

findIndexTest1b :: Proxy (Just 3)
findIndexTest1b = findIndexTest1a

findIndicesTest1a :: Proxy (FindIndices OddSym0 '[1,3,5,2,4,6,7])
findIndicesTest1a = Proxy

findIndicesTest1b :: Proxy '[0,1,2,6]
findIndicesTest1b = findIndicesTest1a

transposeTest1a :: Proxy (Transpose '[[1,2,3]])
transposeTest1a = Proxy

transposeTest1b :: Proxy ('[ '[1], '[2], '[3]])
transposeTest1b = transposeTest1a

transposeTest2a :: Proxy (Transpose '[ '[1], '[2], '[3]])
transposeTest2a = Proxy

transposeTest2b :: Proxy ('[ '[1,2,3]])
transposeTest2b = transposeTest2a
