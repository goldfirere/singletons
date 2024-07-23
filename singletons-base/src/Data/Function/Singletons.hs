{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Function.Singletons
-- Copyright   :  (C) 2016 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines singleton versions of the definitions in @Data.Function@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Function@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Function.Singletons (
    -- * "Prelude" re-exports
    Id, sId, Const, sConst, type (.), (%.), Flip, sFlip, type ($), (%$)
    -- * Other combinators
  , type (&), (%&), On, sOn

    -- * Defunctionalization symbols
  , IdSym0, IdSym1
  , ConstSym0, ConstSym1, ConstSym2
  , type (.@#@$), type (.@#@$$), type (.@#@$$$), type (.@#@$$$$)
  , FlipSym0, FlipSym1, FlipSym2, FlipSym3
  , type ($@#@$), type ($@#@$$), type ($@#@$$$)
  , type (&@#@$), type (&@#@$$), type (&@#@$$$)
  , OnSym0, OnSym1, OnSym2, OnSym3, OnSym4
  ) where

import Data.Singletons.TH
import GHC.Base.Singletons

$(singletonsOnly [d|
  {- GHC falls into a loop here. Not really a surprise.

  -- | @'fix' f@ is the least fixed point of the function @f@,
  -- i.e. the least defined @x@ such that @f x = x@.
  fix :: (a -> a) -> a
  fix f = let x = f x in x
  -}

  -- -| @(*) \`on\` f = \\x y -> f x * f y@.
  --
  -- Typical usage: @'Data.List.sortBy' ('compare' \`on\` 'fst')@.
  --
  -- Algebraic properties:
  --
  -- -* @(*) \`on\` 'id' = (*)@ (if @(*) &#x2209; {&#x22a5;, 'const' &#x22a5;}@)
  --
  -- -* @((*) \`on\` f) \`on\` g = (*) \`on\` (f . g)@
  --
  -- -* @'flip' on f . 'flip' on g = 'flip' on (g . f)@

  -- Proofs (so that I don't have to edit the test-suite):

  --   (*) `on` id
  -- =
  --   \x y -> id x * id y
  -- =
  --   \x y -> x * y
  -- = { If (*) /= _|_ or const _|_. }
  --   (*)

  --   (*) `on` f `on` g
  -- =
  --   ((*) `on` f) `on` g
  -- =
  --   \x y -> ((*) `on` f) (g x) (g y)
  -- =
  --   \x y -> (\x y -> f x * f y) (g x) (g y)
  -- =
  --   \x y -> f (g x) * f (g y)
  -- =
  --   \x y -> (f . g) x * (f . g) y
  -- =
  --   (*) `on` (f . g)
  -- =
  --   (*) `on` f . g

  --   flip on f . flip on g
  -- =
  --   (\h (*) -> (*) `on` h) f . (\h (*) -> (*) `on` h) g
  -- =
  --   (\(*) -> (*) `on` f) . (\(*) -> (*) `on` g)
  -- =
  --   \(*) -> (*) `on` g `on` f
  -- = { See above. }
  --   \(*) -> (*) `on` g . f
  -- =
  --   (\h (*) -> (*) `on` h) (g . f)
  -- =
  --   flip on (g . f)

  on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
  (.*.) `on` f = \x y -> f x .*. f y
  infixl 0 `on`

  -- -| '&' is a reverse application operator.  This provides notational
  -- convenience.  Its precedence is one higher than that of the forward
  -- application operator '$', which allows '&' to be nested in '$'.
  --
  -- @since 4.8.0.0
  (&) :: a -> (a -> b) -> b
  x & f = f x
  infixl 1 &
  |])
