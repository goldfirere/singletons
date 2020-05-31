{- Data/Singletons/Prelude/Util.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file contains helper functions internal to the singletons-base package.
Users of the package should not need to consult this file.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}
module Data.Singletons.Prelude.Util where

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import Data.Semigroup as Semigroup
import Data.Void
import Language.Haskell.TH

-- The list of types that singletons-base processes by default
basicTypes :: [Name]
basicTypes = [ ''Maybe
             , ''[]
             , ''Either
             , ''NonEmpty
             , ''Void
             ] ++ boundedBasicTypes

boundedBasicTypes :: [Name]
boundedBasicTypes =
            [ ''(,)
            , ''(,,)
            , ''(,,,)
            , ''(,,,,)
            , ''(,,,,,)
            , ''(,,,,,,)
            , ''Identity
            ] ++ enumBasicTypes

enumBasicTypes :: [Name]
enumBasicTypes = [ ''Bool, ''Ordering, ''() ]

semigroupBasicTypes :: [Name]
semigroupBasicTypes
  = [ ''Dual
    , ''All
    , ''Any
    , ''Sum
    , ''Product
    -- , ''Endo      see https://github.com/goldfirere/singletons/issues/82
    {- , ''Alt       singletons-th doesn't support higher kinds :(
                     see https://github.com/goldfirere/singletons/issues/150
    -}

    , ''Min
    , ''Max
    , ''Semigroup.First
    , ''Semigroup.Last
    , ''WrappedMonoid
    ]

monoidBasicTypes :: [Name]
monoidBasicTypes
  = [ ''Monoid.First
    , ''Monoid.Last
    ]
