{-# LANGUAGE ExplicitNamespaces, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains everything you need to derive your own singletons via
-- Template Haskell.
--
-- TURN ON @-XScopedTypeVariables@ IN YOUR MODULE IF YOU WANT THIS TO WORK.
--
----------------------------------------------------------------------------

module Data.Singletons.TH (
  -- * Primary Template Haskell generation functions
  singletons, singletonsOnly, genSingletons,
  promote, promoteOnly, genDefunSymbols, genPromotions,

  -- ** Functions to generate equality instances
  promoteEqInstances, promoteEqInstance,
  singEqInstances, singEqInstance,
  singEqInstancesOnly, singEqInstanceOnly,
  singDecideInstances, singDecideInstance,

  -- ** Functions to generate 'Ord' instances
  promoteOrdInstances, promoteOrdInstance,
  singOrdInstances, singOrdInstance,

  -- ** Functions to generate 'Bounded' instances
  promoteBoundedInstances, promoteBoundedInstance,
  singBoundedInstances, singBoundedInstance,

  -- ** Functions to generate 'Enum' instances
  promoteEnumInstances, promoteEnumInstance,
  singEnumInstances, singEnumInstance,

  -- ** Functions to generate 'Show' instances
  promoteShowInstances, promoteShowInstance,
  singShowInstances, singShowInstance,

  -- ** Utility functions
  cases, sCases,

  -- * Basic singleton definitions
  Sing(SFalse, STrue, STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7,
       SLT, SEQ, SGT),
  module Data.Singletons,

  -- * Auxiliary definitions
  -- | These definitions might be mentioned in code generated by Template Haskell,
  -- so they must be in scope.

  PEq(..), If, sIf, type (&&), (%&&), SEq(..),
  POrd(..), SOrd(..), ThenCmp, sThenCmp, Foldl, sFoldl,
  SDecide(..), (:~:)(..), Void, Refuted, Decision(..),
  PBounded(..), SBounded(..),
  PEnum(FromEnum, ToEnum), SEnum(sFromEnum, sToEnum),
  PShow(..), SShow(..),
  ShowString, sShowString, ShowParen, sShowParen, ShowSpace, sShowSpace,
  ShowChar, sShowChar, ShowCommaSpace, sShowCommaSpace,
  (:.), (%.),
  SomeSing(..),

  Error, sError, ErrorSym0, ErrorSym1,
  Undefined, sUndefined, UndefinedSym0,
  TrueSym0, FalseSym0,
  type (==@#@$), type (==@#@$$), type (==@#@$$$),
  type (>@#@$),  type (>@#@$$),  type (>@#@$$$),
  LTSym0, EQSym0, GTSym0,
  Tuple0Sym0,
  Tuple2Sym0, Tuple2Sym1, Tuple2Sym2,
  Tuple3Sym0, Tuple3Sym1, Tuple3Sym2, Tuple3Sym3,
  Tuple4Sym0, Tuple4Sym1, Tuple4Sym2, Tuple4Sym3, Tuple4Sym4,
  Tuple5Sym0, Tuple5Sym1, Tuple5Sym2, Tuple5Sym3, Tuple5Sym4, Tuple5Sym5,
  Tuple6Sym0, Tuple6Sym1, Tuple6Sym2, Tuple6Sym3, Tuple6Sym4, Tuple6Sym5, Tuple6Sym6,
  Tuple7Sym0, Tuple7Sym1, Tuple7Sym2, Tuple7Sym3, Tuple7Sym4, Tuple7Sym5, Tuple7Sym6, Tuple7Sym7,
  CompareSym0, CompareSym1, CompareSym2,
  ThenCmpSym0, ThenCmpSym1, ThenCmpSym2,
  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  MinBoundSym0, MaxBoundSym0,
  ShowsPrecSym0, ShowsPrecSym1, ShowsPrecSym2, ShowsPrecSym3,
  ShowStringSym0, ShowStringSym1, ShowStringSym2,
  ShowParenSym0, ShowParenSym1, ShowParenSym2,
  ShowSpaceSym0, ShowSpaceSym1,
  ShowCharSym0, ShowCharSym1, ShowCharSym2,
  ShowCommaSpaceSym0, ShowCommaSpaceSym1,
  type (.@#@$), type (.@#@$$), type (.@#@$$$), type (.@#@$$$$),
  (:@#@$), (:@#@$$), (:@#@$$$),

  SuppressUnusedWarnings(..)

 ) where

import Data.Singletons
import Data.Singletons.Single
import Data.Singletons.Promote
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Instances
  hiding (Foldl, FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3, sFoldl)
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Foldable
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Show
import Data.Singletons.Decide
import Data.Singletons.TypeLits
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.Names
import Language.Haskell.TH.Desugar

import Language.Haskell.TH
import Data.Singletons.Util
import Control.Arrow ( first )

-- | The function 'cases' generates a case expression where each right-hand side
-- is identical. This may be useful if the type-checker requires knowledge of which
-- constructor is used to satisfy equality or type-class constraints, but where
-- each constructor is treated the same.
cases :: DsMonad q
      => Name        -- ^ The head of the type of the scrutinee. (Like @''Maybe@ or @''Bool@.)
      -> q Exp       -- ^ The scrutinee, in a Template Haskell quote
      -> q Exp       -- ^ The body, in a Template Haskell quote
      -> q Exp
cases tyName expq bodyq = do
  dinfo <- dsReify tyName
  case dinfo of
    Just (DTyConI (DDataD _ _ _ _ _ ctors _) _) ->
      expToTH <$> buildCases (map extractNameArgs ctors) expq bodyq
    Just _ ->
      fail $ "Using <<cases>> with something other than a type constructor: "
              ++ (show tyName)
    _ -> fail $ "Cannot find " ++ show tyName

-- | The function 'sCases' generates a case expression where each right-hand side
-- is identical. This may be useful if the type-checker requires knowledge of which
-- constructor is used to satisfy equality or type-class constraints, but where
-- each constructor is treated the same. For 'sCases', unlike 'cases', the
-- scrutinee is a singleton. But make sure to pass in the name of the /original/
-- datatype, preferring @''Maybe@ over @''SMaybe@.
sCases :: DsMonad q
       => Name        -- ^ The head of the type the scrutinee's type is based on.
                      -- (Like @''Maybe@ or @''Bool@.)
       -> q Exp       -- ^ The scrutinee, in a Template Haskell quote
       -> q Exp       -- ^ The body, in a Template Haskell quote
       -> q Exp
sCases tyName expq bodyq = do
  dinfo <- dsReify tyName
  case dinfo of
    Just (DTyConI (DDataD _ _ _ _ _ ctors _) _) ->
      let ctor_stuff = map (first singDataConName . extractNameArgs) ctors in
      expToTH <$> buildCases ctor_stuff expq bodyq
    Just _ ->
      fail $ "Using <<cases>> with something other than a type constructor: "
              ++ (show tyName)
    _ -> fail $ "Cannot find " ++ show tyName

buildCases :: DsMonad m
           => [(Name, Int)]
           -> m Exp  -- scrutinee
           -> m Exp  -- body
           -> m DExp
buildCases ctor_infos expq bodyq =
  DCaseE <$> (dsExp =<< expq) <*>
             mapM (\con -> DMatch (conToPat con) <$> (dsExp =<< bodyq)) ctor_infos
  where
    conToPat :: (Name, Int) -> DPat
    conToPat (name, num_fields) =
      DConPa name (replicate num_fields DWildPa)
