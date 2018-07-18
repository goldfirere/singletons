{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports the basic definitions to use singletons. For routine
-- use, consider importing 'Data.Singletons.Prelude', which exports constructors
-- for singletons based on types in the @Prelude@.
--
-- You may also want to read
-- the original papers presenting this library, available at
-- <http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf>
-- and <http://cs.brynmawr.edu/~rae/papers/2014/promotion/promotion.pdf>.
--
----------------------------------------------------------------------------

module Data.Singletons (
  -- * Main singleton definitions

  Sing(SLambda, applySing), (@@),

  SingI(..), SingKind(..),

  -- * Working with singletons
  KindOf, SameKind,
  SingInstance(..), SomeSing(..),
  singInstance, pattern Sing, withSingI,
  withSomeSing, pattern FromSing,
  singByProxy, demote,

  singByProxy#,
  withSing, singThat,

  -- ** Defunctionalization
  TyFun, type (~>),
  TyCon1, TyCon2, TyCon3, TyCon4, TyCon5, TyCon6, TyCon7, TyCon8,
  TyCon, Apply, type (@@),

  -- ** Defunctionalized singletons
  -- | When calling a higher-order singleton function, you need to use a
  -- @singFun...@ function to wrap it. See 'singFun1'.
  singFun1, singFun2, singFun3, singFun4, singFun5, singFun6, singFun7,
  singFun8,
  unSingFun1, unSingFun2, unSingFun3, unSingFun4, unSingFun5,
  unSingFun6, unSingFun7, unSingFun8,
  -- $SLambdaPatternSynonyms
  pattern SLambda2, pattern SLambda3, pattern SLambda4, pattern SLambda5,
  pattern SLambda6, pattern SLambda7, pattern SLambda8,

  -- | These type synonyms are exported only to improve error messages; users
  -- should not have to mention them.
  SingFunction1, SingFunction2, SingFunction3, SingFunction4, SingFunction5,
  SingFunction6, SingFunction7, SingFunction8,

  -- * Auxiliary functions
  Proxy(..),

  -- * Defunctionalization symbols
  DemoteSym0, DemoteSym1,
  SameKindSym0, SameKindSym1, SameKindSym2,
  KindOfSym0, KindOfSym1,
  type (~>@#@$), type (~>@#@$$), type (~>@#@$$$),
  ApplySym0, ApplySym1, ApplySym2,
  type (@@@#@$), type (@@@#@$$), type (@@@#@$$$)
  ) where

import Data.Singletons.Internal
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Semigroup
import Data.Singletons.Promote
import Data.Singletons.ShowSing

----------------------------------------------------------------------
---- SomeSing instances ----------------------------------------------
----------------------------------------------------------------------

instance SEq k => Eq (SomeSing k) where
  SomeSing a == SomeSing b = fromSing (a %== b)
  SomeSing a /= SomeSing b = fromSing (a %/= b)

instance SOrd k => Ord (SomeSing k) where
  SomeSing a `compare` SomeSing b = fromSing (a `sCompare` b)
  SomeSing a <         SomeSing b = fromSing (a %<  b)
  SomeSing a <=        SomeSing b = fromSing (a %<= b)
  SomeSing a >         SomeSing b = fromSing (a %>  b)
  SomeSing a >=        SomeSing b = fromSing (a %>= b)

instance SBounded k => Bounded (SomeSing k) where
  minBound = SomeSing sMinBound
  maxBound = SomeSing sMaxBound

instance (SEnum k, SingKind k) => Enum (SomeSing k) where
  succ (SomeSing a) = SomeSing (sSucc a)
  pred (SomeSing a) = SomeSing (sPred a)
  toEnum n = withSomeSing (fromIntegral n) (SomeSing . sToEnum)
  fromEnum (SomeSing a) = fromIntegral (fromSing (sFromEnum a))
  enumFromTo (SomeSing from) (SomeSing to) =
    map toSing (fromSing (sEnumFromTo from to))
  enumFromThenTo (SomeSing from) (SomeSing then_) (SomeSing to) =
    map toSing (fromSing (sEnumFromThenTo from then_ to))

instance SNum k => Num (SomeSing k) where
  SomeSing a + SomeSing b = SomeSing (a %+ b)
  SomeSing a - SomeSing b = SomeSing (a %- b)
  SomeSing a * SomeSing b = SomeSing (a %* b)
  negate (SomeSing a) = SomeSing (sNegate a)
  abs    (SomeSing a) = SomeSing (sAbs a)
  signum (SomeSing a) = SomeSing (sSignum a)
  fromInteger n = withSomeSing (fromIntegral n) (SomeSing . sFromInteger)

deriving instance ShowSing k => Show (SomeSing k)

instance SSemigroup k => Semigroup (SomeSing k) where
  SomeSing a <> SomeSing b = SomeSing (a %<> b)

instance SMonoid k => Monoid (SomeSing k) where
  mempty = SomeSing sMempty

----------------------------------------------------------------------
---- Defunctionalization symbols -------------------------------------
----------------------------------------------------------------------

$(genDefunSymbols [''Demote, ''SameKind, ''KindOf, ''(~>), ''Apply, ''(@@)])
-- SingFunction1 et al. are not defunctionalizable at the moment due to #198

{- $SLambdaPatternSynonyms

@SLambda{2...8}@ are explicitly bidirectional pattern synonyms for
defunctionalized singletons (@'Sing' (f :: k '~>' k' '~>' k'')@).

As __constructors__: Same as @singFun{2..8}@. For example, one can turn a
binary function on singletons @sTake :: 'SingFunction2' TakeSym0@ into a
defunctionalized singleton @'Sing' (TakeSym :: Nat '~>' [a] '~>' [a])@:

@
>>> import Data.Singletons.Prelude.List
>>> :set -XTypeApplications
>>>
>>> :t 'SLambda2'
'SLambda2' :: 'SingFunction2' f -> 'Sing' f
>>> :t 'SLambda2' \@TakeSym0
'SLambda2' :: 'SingFunction2' TakeSym0 -> 'Sing' TakeSym0
>>> :t 'SLambda2' \@TakeSym0 sTake
'SLambda2' :: 'Sing' TakeSym0
@

This is useful for functions on singletons that expect a defunctionalized
singleton as an argument, such as @sZipWith :: 'SingFunction3' ZipWithSym0@:

@
sZipWith :: Sing (f :: a '~>' b '~>' c) -> Sing (xs :: [a]) -> Sing (ys :: [b]) -> Sing (ZipWith f xs ys :: [c])
sZipWith ('SLambda2' \@TakeSym0 sTake) :: Sing (xs :: [Nat]) -> Sing (ys :: [[a]]) -> Sing (ZipWith TakeSym0 xs ys :: [[a]])
@

As __patterns__: Same as @unSingFun{2..8}@. Gets a binary term-level
Haskell function on singletons
@'Sing' (x :: k) -> 'Sing' (y :: k') -> 'Sing' (f \@\@ x \@\@ y)@
from a defunctionalised @'Sing' f@. Alternatively, as a record field accessor:

@
applySing2 :: 'Sing' (f :: k '~>' k' '~>' k'') -> 'SingFunction2' f
@
-}
