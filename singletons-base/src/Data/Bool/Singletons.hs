{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bool.Singletons
-- Copyright   :  (C) 2013-2014 Richard Eisenberg, Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for 'Bool',
-- including singled versions of all the definitions in @Data.Bool@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Bool@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Bool.Singletons (
  -- * The 'Bool' singleton
  Sing, SBool(..),

  -- * Conditionals
  If, sIf,

  -- * Singletons from @Data.Bool@
  Not, sNot, type (&&), type (||), (%&&), (%||),

  -- | The following are derived from the function 'bool' in @Data.Bool@. The extra
  -- underscore is to avoid name clashes with the type 'Bool'.
  bool_, Bool_, sBool_, Otherwise, sOtherwise,

  -- * Defunctionalization symbols
  TrueSym0, FalseSym0,

  IfSym0, IfSym1, IfSym2, IfSym3,
  NotSym0, NotSym1,
  type (&&@#@$), type (&&@#@$$), type (&&@#@$$$),
  type (||@#@$), type (||@#@$$), type (||@#@$$$),
  Bool_Sym0, Bool_Sym1, Bool_Sym2, Bool_Sym3,
  OtherwiseSym0
  ) where

import Data.Singletons
import Data.Singletons.Base.Instances
import Data.Singletons.TH
import Data.Type.Bool ( If, type (&&), type (||), Not )

$(singletons [d|
  bool_ :: a -> a -> Bool -> a
  bool_ fls _tru False = fls
  bool_ _fls tru True  = tru
 |])

$(singletonsOnly [d|
  otherwise               :: Bool
  otherwise               =  True
  |])

-- | Conjunction of singletons
(%&&) :: Sing a -> Sing b -> Sing (a && b)
SFalse %&& _ = SFalse
STrue  %&& a = a
infixr 3 %&&
$(genDefunSymbols [''(&&)])
instance SingI (&&@#@$) where
  sing = singFun2 (%&&)
instance SingI x => SingI ((&&@#@$$) x) where
  sing = singFun1 (sing @x %&&)

-- | Disjunction of singletons
(%||) :: Sing a -> Sing b -> Sing (a || b)
SFalse %|| a = a
STrue  %|| _ = STrue
infixr 2 %||
$(genDefunSymbols [''(||)])
instance SingI (||@#@$) where
  sing = singFun2 (%||)
instance SingI x => SingI ((||@#@$$) x) where
  sing = singFun1 (sing @x %||)

-- | Negation of a singleton
sNot :: Sing a -> Sing (Not a)
sNot SFalse = STrue
sNot STrue  = SFalse
$(genDefunSymbols [''Not])
instance SingI NotSym0 where
  sing = singFun1 sNot

-- | Conditional over singletons
sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c
$(genDefunSymbols [''If])
instance SingI IfSym0 where
  sing = singFun3 sIf
instance SingI c => SingI (IfSym1 c) where
  sing = singFun2 $ sIf (sing @c)
instance (SingI c, SingI t) => SingI (IfSym2 c t) where
  sing = singFun1 $ sIf (sing @c) (sing @t)
instance SingI1 IfSym1 where
  liftSing s = singFun2 $ sIf s
instance SingI c => SingI1 (IfSym2 c) where
  liftSing s = singFun1 $ sIf (sing @c) s
instance SingI2 IfSym2 where
  liftSing2 s1 s2 = singFun1 $ sIf s1 s2
