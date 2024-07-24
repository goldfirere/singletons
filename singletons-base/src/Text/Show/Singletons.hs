{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Singletons
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the SShow singleton version of the Show type class.
--
-----------------------------------------------------------------------------

module Text.Show.Singletons (
  PShow(..), SShow(..), SymbolS, show_,
  Shows, sShows,
  ShowListWith, sShowListWith,
  ShowChar, sShowChar,
  ShowString, sShowString,
  ShowParen, sShowParen,
  ShowSpace, sShowSpace,
  ShowCommaSpace, sShowCommaSpace,
  AppPrec, sAppPrec,
  AppPrec1, sAppPrec1,

  -- * Defunctionalization symbols
  ShowsPrecSym0, ShowsPrecSym1, ShowsPrecSym2, ShowsPrecSym3,
  Show_Sym0, Show_Sym1,
  ShowListSym0, ShowListSym1, ShowListSym2,
  ShowsSym0, ShowsSym1, ShowsSym2,
  ShowListWithSym0, ShowListWithSym1, ShowListWithSym2, ShowListWithSym3,
  ShowCharSym0, ShowCharSym1, ShowCharSym2,
  ShowStringSym0, ShowStringSym1, ShowStringSym2,
  ShowParenSym0, ShowParenSym1, ShowParenSym2,
  ShowSpaceSym0, ShowSpaceSym1,
  ShowCommaSpaceSym0, ShowCommaSpaceSym1,
  AppPrecSym0, AppPrec1Sym0
  ) where

import           Data.Bool.Singletons
import           Data.Eq.Singletons
import           Data.Kind
import           Data.List.NonEmpty (NonEmpty)
import           Data.List.Singletons.Internal
import           Data.Ord (Down)
import           Data.Ord.Singletons
import           Data.Semigroup.Singletons.Internal.Classes
import           Data.Singletons
import           Data.Singletons.Base.Instances
import           Data.Singletons.TH
import qualified Data.Text as T
import           GHC.Base.Singletons
import           GHC.Num.Singletons
import           GHC.TypeLits
import           GHC.TypeLits.Singletons
import qualified Prelude as P
import           Prelude hiding (Show(..))
import           Unsafe.Coerce (unsafeCoerce)

-- | The @shows@ functions return a function that prepends the
-- output 'Symbol' to an existing 'Symbol'.  This allows constant-time
-- concatenation of results using function composition.
type SymbolS :: Type
type SymbolS = Symbol -> Symbol

$(singletonsOnly [d|
  class Show a where
    showsPrec :: Natural -> a -> SymbolS
    show_     :: a -> Symbol
    showList  :: [a] -> SymbolS

    showsPrec _ x s = show_ x <> s
    show_ x         = shows x ""
    showList ls   s = showListWith shows ls s

  shows :: Show a => a -> SymbolS
  shows s = showsPrec 0 s

  showListWith :: (a -> SymbolS) -> [a] -> SymbolS
  showListWith _     []     s = "[]" <> s
  showListWith showx (x:xs) s = "["  <> showx x (showl xs)
    where
      showl []     = "]" <> s
      showl (y:ys) = "," <> showx y (showl ys)

  showChar :: Char -> SymbolS
  showChar = consSymbol

  showString :: Symbol -> SymbolS
  showString = (<>)

  showParen :: Bool -> SymbolS -> SymbolS
  showParen b p = if b then showChar '(' . p . showChar ')' else p

  showSpace :: SymbolS
  showSpace = \xs -> " " <> xs

  showCommaSpace :: SymbolS
  showCommaSpace = showString ", "

  appPrec, appPrec1 :: Nat
  appPrec  = 10
  appPrec1 = 11

  instance Show a => Show [a] where
    showsPrec _ = showList

  show_tuple :: [SymbolS] -> SymbolS
  show_tuple ss = showChar '('
                . foldr1 (\s r -> s . showChar ',' . r) ss
                . showChar ')'

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

  deriving instance Show a => Show (Down a)
  |])

$(promoteOnly [d|
  showsNat :: Natural -> SymbolS
  showsNat 0 = showChar '0'
  showsNat 1 = showChar '1'
  showsNat 2 = showChar '2'
  showsNat 3 = showChar '3'
  showsNat 4 = showChar '4'
  showsNat 5 = showChar '5'
  showsNat 6 = showChar '6'
  showsNat 7 = showChar '7'
  showsNat 8 = showChar '8'
  showsNat 9 = showChar '9'
  showsNat n = showsNat (n `div` 10) . showsNat (n `mod` 10)
  |])

instance PShow Natural where
  type ShowsPrec _ n x = ShowsNat n x

instance SShow Natural where
  sShowsPrec _ sn sx =
    let n = fromSing sn
        x = fromSing sx
    in withSomeSSymbol (P.show n ++ T.unpack x) unsafeCoerce

$(promoteOnly [d|
  showsCharPrec :: Natural -> Char -> SymbolS
  showsCharPrec _ '\'' = showString "'\\''"
  showsCharPrec _ c    = showChar '\'' . showLitChar c . showChar '\''

  showCharList :: [Char] -> SymbolS
  showCharList cs = showChar '"' . showLitString cs . showChar '"'

  -- -| Like 'showCharList', but for 'Symbol's.
  showSymbol :: Symbol -> SymbolS
  showSymbol sym = showChar '"' . showLitSymbol sym . showChar '"'

  -- -| Convert a character to a string using only printable characters,
  -- using Haskell source-language escape conventions.  For example:
  --
  -- > showLitChar '\n' s  =  "\\n" ++ s
  --
  showLitChar                :: Char -> SymbolS
  showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (charToNat c)) s)
  showLitChar '\DEL'         s =  showString "\\DEL" s
  showLitChar '\\'           s =  showString "\\\\" s
  showLitChar c s | c >= ' '   =  showChar c s
  showLitChar '\a'           s =  showString "\\a" s
  showLitChar '\b'           s =  showString "\\b" s
  showLitChar '\f'           s =  showString "\\f" s
  showLitChar '\n'           s =  showString "\\n" s
  showLitChar '\r'           s =  showString "\\r" s
  showLitChar '\t'           s =  showString "\\t" s
  showLitChar '\v'           s =  showString "\\v" s
  showLitChar '\SO'          s =  protectEsc (== 'H') (showString "\\SO") s
  showLitChar c              s =  showString ('\\' `consSymbol` (asciiTab!!charToNat c)) s
          -- I've done manual eta-expansion here, because otherwise it's
          -- impossible to stop (asciiTab!!charToNat) getting floated out as an MFE

  showLitString :: String -> SymbolS
  -- -| Same as 'showLitChar', but for strings
  -- It converts the string to a string using Haskell escape conventions
  -- for non-printable characters. Does not add double-quotes around the
  -- whole thing; the caller should do that.
  -- The main difference from showLitChar (apart from the fact that the
  -- argument is a string not a list) is that we must escape double-quotes
  showLitString []         s = s
  showLitString ('"' : cs) s = showString "\\\"" (showLitString cs s)
  showLitString (c   : cs) s = showLitChar c (showLitString cs s)
     -- Making 's' an explicit parameter makes it clear to GHC that
     -- showLitString has arity 2, which avoids it allocating an extra lambda
     -- The sticking point is the recursive call to (showLitString cs), which
     -- it can't figure out would be ok with arity 2.

  -- -| Like 'showLitString', but for 'Symbol's.
  showLitSymbol :: Symbol -> SymbolS
  showLitSymbol sym s = case unconsSymbol sym of
    Nothing        -> s
    Just ('"', cs) -> showString "\\\"" (showLitSymbol cs s)
    Just (c,   cs) -> showLitChar c (showLitSymbol cs s)

  isDec :: Char -> Bool
  isDec c = c >= '0' && c <= '9'

  protectEsc :: (Char -> Bool) -> SymbolS -> SymbolS
  protectEsc p f             = f . cont
                               -- where cont s@(c:_) | p c = "\\&" ++ s
                               --       cont s             = s
                               where cont s = case unconsSymbol s of
                                       Just (c, _) | p c -> "\\&" <> s
                                       Nothing           -> s

  asciiTab :: [Symbol]
  asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
             ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
              "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
              "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
              "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
              "SP"]
  |])

instance PShow Char where
  type ShowsPrec p c x = ShowsCharPrec p c x
  type ShowList cs x = ShowCharList cs x

instance SShow Char where
  sShowsPrec sp sc sx =
    let p  = fromSing sp
        c  = fromSing sc
        x  = fromSing sx
    in withSomeSSymbol (P.showsPrec (fromIntegral p) c (T.unpack x)) unsafeCoerce

  sShowList scs sx =
    let cs = fromSing scs
        x  = fromSing sx
    in withSomeSSymbol (P.showList cs (T.unpack x)) unsafeCoerce

instance PShow Symbol where
  type ShowsPrec _ s x = ShowSymbol s x

instance SShow Symbol where
  sShowsPrec _ ss sx =
    let s  = fromSing ss
        x  = fromSing sx
    in withSomeSSymbol (P.show s ++ T.unpack x) unsafeCoerce

-- | 'P.show', but with an extra underscore so that its promoted counterpart
-- ('Show_') will not clash with the 'Show' class.
show_ :: P.Show a => a -> String
show_ = P.show

$(singShowInstances [ ''(), ''Maybe, ''Either, ''NonEmpty, ''Bool,
                      ''Ordering, ''Void ])
