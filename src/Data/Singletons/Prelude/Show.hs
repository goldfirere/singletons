{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Show
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the SShow singleton version of the Show type class.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Show (
  PShow(..), SShow(..), SymbolS, SChar,

  ShowsPrecSym0, ShowsPrecSym1, ShowsPrecSym2, ShowsPrecSym3,
  Show'Sym0, Show'Sym1,
  ShowListSym0, ShowListSym1, ShowListSym2,

  (:<>), (%:<>),
  (:<>$), (:<>$$), (:<>$$$),

  Shows, sShows,
  ShowsSym0, ShowsSym1, ShowsSym2,

  ShowListWith, sShowListWith,
  ShowListWithSym0, ShowListWithSym1, ShowListWithSym2, ShowListWithSym3,

  ShowChar, sShowChar,
  ShowCharSym0, ShowCharSym1, ShowCharSym2,

  ShowString, sShowString,
  ShowStringSym0, ShowStringSym1, ShowStringSym2,

  ShowParen, sShowParen,
  ShowParenSym0, ShowParenSym1, ShowParenSym2,

  ShowSpace, sShowSpace,
  ShowSpaceSym0, ShowSpaceSym1,

  ShowCommaSpace, sShowCommaSpace,
  ShowCommaSpaceSym0, ShowCommaSpaceSym1,

  AppPrec, sAppPrec, AppPrecSym0,

  AppPrec1, sAppPrec1, AppPrec1Sym0
  ) where

import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Monoid ((<>))
import           Data.Singletons.Prelude.Base
import           Data.Singletons.Prelude.Instances
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Ord
import           Data.Singletons.Single
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import qualified Data.Text as T

import           GHC.TypeLits

import qualified Prelude as P
import           Prelude hiding (Show(..))

import           Unsafe.Coerce (unsafeCoerce)

-- | The promoted analogue of '(<>)' for 'Symbol's. This uses the special
-- 'AppendSymbol' type family from "GHC.TypeLits".
type a :<> b = AppendSymbol a b
infixr 6 :<>

-- | The singleton analogue of '(<>)' for 'Symbol's.
(%:<>) :: Sing a -> Sing b -> Sing (a :<> b)
sa %:<> sb =
    let a  = fromSing sa
        b  = fromSing sb
        ex = someSymbolVal $ T.unpack $ a <> b
    in case ex of
         SomeSymbol (_ :: Proxy ab) -> unsafeCoerce (SSym :: Sing ab)
infixr 6 %:<>

type (:<>$$$) (x :: Symbol) (y :: Symbol) =
    (:<>) x y
instance SuppressUnusedWarnings (:<>$$) where
  suppressUnusedWarnings = snd ((:<>$$###), ())
data (:<>$$) (x :: Symbol) (y :: TyFun Symbol Symbol)
  = forall arg. KindOf (Apply ((:<>$$) x) arg) ~ KindOf ((:<>$$$) x arg) => (:<>$$###)
type instance Apply ((:<>$$) x) y = (:<>$$$) x y
instance SuppressUnusedWarnings (:<>$) where
  suppressUnusedWarnings = snd ((:<>$###), ())
data (:<>$) (x :: TyFun Symbol (TyFun Symbol Symbol -> Type))
  = forall arg. KindOf (Apply (:<>$) arg) ~ KindOf ((:<>$$) arg) => (:<>$###)
type instance Apply (:<>$) x = (:<>$$) x

-- | The @shows@ functions return a function that prepends the
-- output 'Symbol' to an existing 'Symbol'.  This allows constant-time
-- concatenation of results using function composition.
type SymbolS = Symbol -> Symbol

-- | GHC currently has no notion of type-level 'Char's, so we fake them with
-- single-character 'Symbol's.
type SChar = Symbol

$(singletonsOnly [d|
  class Show a where
    showsPrec :: Nat -> a -> SymbolS
    -- Intentionally renamed to avoid clashing with the Show class
    show'     :: a -> Symbol
    showList  :: [a] -> SymbolS

    showsPrec _ x s = show' x <> s
    show' x         = shows x ""
    showList ls   s = showListWith shows ls s

  shows :: Show a => a -> SymbolS
  shows s = showsPrec 0 s

  showListWith :: (a -> SymbolS) -> [a] -> SymbolS
  showListWith _     []     s = "[]" <> s
  showListWith showx (x:xs) s = "["  <> showx x (showl xs)
    where
      showl []     = "]" <> s
      showl (y:ys) = "," <> showx y (showl ys)

  showChar :: SChar -> SymbolS
  showChar = (<>)

  showString :: Symbol -> SymbolS
  showString = (<>)

  showParen :: Bool -> SymbolS -> SymbolS
  showParen b p = if b then showChar "(" . p . showChar ")" else p

  showSpace :: SymbolS
  showSpace = \xs -> " " <> xs

  showCommaSpace :: SymbolS
  showCommaSpace = showString ", "

  appPrec, appPrec1 :: Nat
  appPrec  = 10
  appPrec1 = 11

  instance Show a => Show [a] where
    showsPrec _ = showList

  -- -| This is not an ideal Show instance for Symbol, since the Show instance
  -- for String escapes special characters. Unfortunately, GHC lacks the ability
  -- to case on individual characters in a Symbol (at least, not without GHC
  -- plugins), so this is the best we can do for the time being.
  instance Show Symbol where
    showsPrec _ = showString

  show_tuple :: [SymbolS] -> SymbolS
  show_tuple ss = showChar "("
                . foldr1 (\s r -> s . showChar "," . r) ss
                . showChar ")"

  instance (Show a, Show b) => Show (a,b)  where
    showsPrec _ (a,b) s = show_tuple [shows a, shows b] s

  instance (Show a, Show b, Show c) => Show (a, b, c) where
    showsPrec _ (a,b,c) s = show_tuple [shows a, shows b, shows c] s

  instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showsPrec _ (a,b,c,d) s = show_tuple [shows a, shows b, shows c, shows d] s

  instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showsPrec _ (a,b,c,d,e) s = show_tuple [shows a, shows b, shows c, shows d, shows e] s

  instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where
    showsPrec _ (a,b,c,d,e,f) s = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f] s

  instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
          => Show (a,b,c,d,e,f,g) where
    showsPrec _ (a,b,c,d,e,f,g) s
          = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g] s
  |])

-- At the moment, I can't think of a way to implement a PShow Nat instance,
-- since that would require a way to convert Nats to Symbols at the type level.
-- Oh well.

instance SShow Nat where
  sShowsPrec _ sn sx =
    let n = fromSing sn
        x = fromSing sx
        ex = someSymbolVal (P.show n ++ T.unpack x)
    in
    case ex of
      SomeSymbol (_ :: Proxy s) -> unsafeCoerce (SSym :: Sing s)

  -- Annoyingly enough, the default definitions for sShow' and sShowList won't
  -- typecheck because they rely on the type-level Show' and ShowList reducing,
  -- but since we don't have a PShow Nat instance, that doesn't happen. To work
  -- around this, we define sShow' and sShowList by hand.

  sShow' sn =
    let n = fromSing sn
        ex = someSymbolVal (P.show n)
    in
    case ex of
      SomeSymbol (_ :: Proxy s) -> unsafeCoerce (SSym :: Sing s)

  sShowList sl sx =
    let l = fromSing sl
        x = fromSing sx
        ex = someSymbolVal (P.show l ++ T.unpack x)
    in
    case ex of
      SomeSymbol (_ :: Proxy s) -> unsafeCoerce (SSym :: Sing s)

$(singShowInstances [ ''(), ''Maybe, ''Either, ''NonEmpty, ''Bool, ''Ordering ])
