{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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

  Sing, SLambda(..), (@@),

  SingI(..), SingKind(..),

  -- * Working with singletons
  KindOf, SameKind,
  SingInstance(..), SomeSing(..),
  singInstance, pattern Sing, withSingI,
  withSomeSing, pattern FromSing,
  singByProxy, demote,

  singByProxy#,
  withSing, singThat,

  -- ** @WrappedSing@
  WrappedSing(..), SWrappedSing(..), UnwrapSing,
  -- $SingletonsOfSingletons

  -- ** Defunctionalization
  TyFun, type (~>),
  TyCon1, TyCon2, TyCon3, TyCon4, TyCon5, TyCon6, TyCon7, TyCon8,
  TyCon, Apply, type (@@), ApplyTyCon, ApplyTyConAux1, ApplyTyConAux2,

  -- ** Defunctionalized singletons
  -- | When calling a higher-order singleton function, you need to use a
  -- @singFun...@ function to wrap it. See 'singFun1'.
  singFun1, singFun2, singFun3, singFun4, singFun5, singFun6, singFun7,
  singFun8,
  unSingFun1, unSingFun2, unSingFun3, unSingFun4, unSingFun5,
  unSingFun6, unSingFun7, unSingFun8,
  -- $SLambdaPatternSynonyms
  pattern SLambda2, applySing2,
  pattern SLambda3, applySing3,
  pattern SLambda4, applySing4,
  pattern SLambda5, applySing5,
  pattern SLambda6, applySing6,
  pattern SLambda7, applySing7,
  pattern SLambda8, applySing8,

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
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.IsString
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Semigroup
import Data.Singletons.Promote
import Data.Singletons.ShowSing
import Data.Singletons.Single (singITyConInstances)
import Data.String
import qualified Data.Text as T (pack)

----------------------------------------------------------------------
---- SingI TyCon{N} instances ----------------------------------------
----------------------------------------------------------------------

{-
Generates SingI instances for TyCon1 through TyCon8:

  instance (forall a.    SingI a           => SingI (f a),   ...) => SingI (TyCon1 f)
  instance (forall a b. (SingI a, SingI b) => SingI (f a b), ...) => SingI (TyCon2 f)
  ...
-}
$(singITyConInstances [1..8])

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

instance SEnum k => Enum (SomeSing k) where
  succ (SomeSing a) = SomeSing (sSucc a)
  pred (SomeSing a) = SomeSing (sPred a)
  toEnum n = withSomeSing (fromIntegral n) (SomeSing . sToEnum)
  fromEnum (SomeSing a) = fromIntegral (fromSing (sFromEnum a))
  enumFromTo (SomeSing from) (SomeSing to) =
    listFromSingShallow (sEnumFromTo from to)
  enumFromThenTo (SomeSing from) (SomeSing then_) (SomeSing to) =
    listFromSingShallow (sEnumFromThenTo from then_ to)

-- Like the 'fromSing' implementation for lists, but bottoms out at
-- 'SomeSing' instead of recursively invoking 'fromSing'.
listFromSingShallow :: SList (x :: [a]) -> [SomeSing a]
listFromSingShallow SNil         = []
listFromSingShallow (SCons x xs) = SomeSing x : listFromSingShallow xs

instance SNum k => Num (SomeSing k) where
  SomeSing a + SomeSing b = SomeSing (a %+ b)
  SomeSing a - SomeSing b = SomeSing (a %- b)
  SomeSing a * SomeSing b = SomeSing (a %* b)
  negate (SomeSing a) = SomeSing (sNegate a)
  abs    (SomeSing a) = SomeSing (sAbs a)
  signum (SomeSing a) = SomeSing (sSignum a)
  fromInteger n = withSomeSing (fromIntegral n) (SomeSing . sFromInteger)

instance ShowSing k => Show (SomeSing k) where
  showsPrec p (SomeSing (s :: Sing a)) =
    showParen (p > 10) $ showString "SomeSing " . showsPrec 11 s
      :: ShowSing' a => ShowS

instance SSemigroup k => Semigroup (SomeSing k) where
  SomeSing a <> SomeSing b = SomeSing (a %<> b)

instance SMonoid k => Monoid (SomeSing k) where
  mempty = SomeSing sMempty

instance SIsString k => IsString (SomeSing k) where
  fromString s = withSomeSing (T.pack s) (SomeSing . sFromString)

----------------------------------------------------------------------
---- Defunctionalization symbols -------------------------------------
----------------------------------------------------------------------

$(genDefunSymbols [''Demote, ''SameKind, ''KindOf, ''(~>), ''Apply, ''(@@)])
-- WrapSing, UnwrapSing, and SingFunction1 et al. are not defunctionalizable
-- at the moment due to Trac #9269

{- $SingletonsOfSingletons

Aside from being a data type to hang instances off of, 'WrappedSing' has
another purpose as a general-purpose mechanism for allowing one to write
code that uses singletons of other singletons. For instance, suppose you
had the following data type:

@
data T :: Type -> Type where
  MkT :: forall a (x :: a). 'Sing' x -> F a -> T a
@

A naïve attempt at defining a singleton for @T@ would look something like
this:

@
data ST :: forall a. T a -> Type where
  SMkT :: forall a (x :: a) (sx :: 'Sing' x) (f :: F a).
          'Sing' sx -> 'Sing' f -> ST (MkT sx f)
@

But there is a problem here: what exactly /is/ @'Sing' sx@? If @x@ were 'True',
for instance, then @sx@ would be 'STrue', but it's not clear what
@'Sing' 'STrue'@ should be. One could define @SSBool@ to be the singleton of
'SBool's, but in order to be thorough, one would have to generate a singleton
for /every/ singleton type out there. Plus, it's not clear when to stop. Should
we also generate @SSSBool@, @SSSSBool@, etc.?

Instead, 'WrappedSing' and its singleton 'SWrappedSing' provide a way to talk
about singletons of other arbitrary singletons without the need to generate a
bazillion instances. For reference, here is the definition of 'SWrappedSing':

@
newtype 'SWrappedSing' :: forall k (a :: k). 'WrappedSing' a -> Type where
  'SWrapSing' :: forall k (a :: k) (ws :: 'WrappedSing' a).
                 { 'sUnwrapSing' :: 'Sing' a } -> 'SWrappedSing' ws
type instance 'Sing' \@('WrappedSing' a) = 'SWrappedSing'
@

'SWrappedSing' is a bit of an unusual singleton in that its field is a
singleton for @'Sing' \@k@, not @'WrappedSing' \@k@. But that's exactly the
point—a singleton of a singleton contains as much type information as the
underlying singleton itself, so we can get away with just @'Sing' \@k@.

As an example of this in action, here is how you would define the singleton
for the earlier @T@ type:

@
data ST :: forall a. T a -> Type where
  SMkT :: forall a (x :: a) (sx :: 'Sing' x) (f :: F a).
          'Sing' ('WrapSing' sx) -> 'Sing' f -> ST (MkT sx f)
@

With this technique, we won't need anything like @SSBool@ in order to
instantiate @x@ with 'True'. Instead, the field of type
@'Sing' ('WrapSing' sx)@ will simply be a newtype around 'SBool'. In general,
you'll need /n/ layers of 'WrapSing' if you wish to single a singleton /n/
times.

Note that this is not the only possible way to define a singleton for @T@.
An alternative approach that does not make use of singletons-of-singletons is
discussed at some length
<https://github.com/goldfirere/singletons/issues/366#issuecomment-489469086 here>.
Due to the technical limitations of this approach, however, we do not use it
in @singletons@ at the moment, instead favoring the
slightly-clunkier-but-more-reliable 'WrappedSing' approach.
-}

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
