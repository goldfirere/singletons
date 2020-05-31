module T200 where

import Data.Singletons.Prelude.TH

$(singletons [d|
      data ErrorMessage = ErrorMessage :$$: ErrorMessage
                        | ErrorMessage :<>: ErrorMessage
                        | EM [Bool]

      ($$:) :: ErrorMessage -> ErrorMessage -> ErrorMessage
      x $$: y = x :$$: y

      (<>:) :: ErrorMessage -> ErrorMessage -> ErrorMessage
      x <>: y = x :<>: y
    |])
