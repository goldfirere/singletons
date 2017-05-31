{-# LANGUAGE TemplateHaskell, KindSignatures, PolyKinds, TypeOperators,
             DataKinds, ScopedTypeVariables, TypeFamilies, GADTs,
             UndecidableInstances, BangPatterns, DefaultSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Base
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements singletonized versions of functions from @GHC.Base@ module.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Tuple@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Base (
  -- * Basic functions
  Foldr, sFoldr, Map, sMap, (:++), (%:++), Otherwise, sOtherwise,
  Id, sId, Const, sConst, (:.), (%:.), type ($), type ($!), (%$), (%$!),
  Flip, sFlip, AsTypeOf, sAsTypeOf,
  Seq, sSeq,
  PMonoid(..), SMonoid(..),
  PFunctor(..), SFunctor(..),
  PApplicative(..), SApplicative(..),
  (:<**>), (%:<**>),
  LiftA, sLiftA, LiftA3, sLiftA3, Join, sJoin,
  PMonad(..), SMonad(..),

  -- * Defunctionalization symbols
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  MapSym0, MapSym1, MapSym2,
  (:++$), (:++$$), (:++$$$),
  OtherwiseSym0,
  IdSym0, IdSym1,
  ConstSym0, ConstSym1, ConstSym2,
  (:.$), (:.$$), (:.$$$), (:.$$$$),
  type ($$), type ($$$), type ($$$$),
  type ($!$), type ($!$$), type ($!$$$),
  FlipSym0, FlipSym1, FlipSym2, FlipSym3,
  AsTypeOfSym0, AsTypeOfSym1, AsTypeOfSym2,
  SeqSym0, SeqSym1, SeqSym2,
  MemptySym0,
  MappendSym0, MappendSym1, MappendSym2,
  MConcatSym0, MconcatSym1,
  FMapSym0, FMapSym1, FMapSym2,
  (:<$$), (:<$$$), (:<$$$$),
  PureSym0, PureSym1,
  (:<*>$), (:<*>$$), (:<*>$$$),
  LiftA2Sym0, LiftA2Sym1, LiftA2Sym2, LiftA2Sym3,
  (:*>$), (:*>$$), (:*>$$$),
  (:<*$), (:<*$$), (:<*$$$),
  (:<**>$), (:<**>$$), (:<**>$$$),
  LiftASym0, LiftASym1, LiftASym2,
  LiftA3Sym0, LiftA3Sym1, LiftA3Sym2, LiftA3Sym3, LiftA3Sym4,
  JoinSym0, JoinSym1,
  (:>>=$), (:>>=$$), (:>>=$$$),
  ReturnSym0, ReturnSym1,
  FailSym0, FailSym1
  ) where

import Data.Singletons.Prelude.Instances
import Data.Singletons.Single
import Data.Singletons
import Data.Singletons.Prelude.Bool
import Data.Singletons.TypeLits.Internal
import Data.Singletons.Promote

import Data.Kind ( Type )

-- Promoted and singletonized versions of "otherwise" are imported and
-- re-exported from Data.Singletons.Prelude.Bool. This is done to avoid cyclic
-- module dependencies.

$(singletonsOnly [d|
  foldr                   :: (a -> b -> b) -> b -> [a] -> b
  foldr k z = go
            where
              go []     = z
              go (y:ys) = y `k` go ys

  map                     :: (a -> b) -> [a] -> [b]
  map _ []                = []
  map f (x:xs)            = f x : map f xs

  (++)                    :: [a] -> [a] -> [a]
  (++) []     ys          = ys
  (++) (x:xs) ys          = x : xs ++ ys
  infixr 5 ++

  id                      :: a -> a
  id x                    =  x

  const                   :: a -> b -> a
  const x _               =  x

  (.)    :: (b -> c) -> (a -> b) -> a -> c
  (.) f g = \x -> f (g x)
  infixr 9 .

  flip                    :: (a -> b -> c) -> b -> a -> c
  flip f x y              =  f y x

  asTypeOf                :: a -> a -> a
  asTypeOf                =  const

  -- This is not part of GHC.Base, but we need to emulate seq and this is a good
  -- place to do it.
  seq :: a -> b -> b
  seq _ x = x
  infixr 0 `seq`

  class Monoid a where
        mempty  :: a
        -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
        -- ^ An associative operation
        mconcat :: [a] -> a

        -- ^ Fold a list using the monoid.
        -- For most types, the default definition for 'mconcat' will be
        -- used, but the function is included in the class definition so
        -- that an optimized version can be provided for specific types.

        mconcat = foldr mappend mempty

  instance Monoid [a] where
        mempty  = []
        mappend = (++)
        mconcat xss = [x | xs <- xss, x <- xs]

  -- this one fails because mempty is applied to an argument, and singletons
  -- isn't expecting that. I haven't checked whether this is easy to fix.
  {-
  instance Monoid b => Monoid (a -> b) where
        mempty _ = mempty
        mappend f g x = f x `mappend` g x
  -}

  instance (Monoid a, Monoid b) => Monoid (a,b) where
          mempty = (mempty, mempty)
          (a1,b1) `mappend` (a2,b2) =
                  (a1 `mappend` a2, b1 `mappend` b2)


  instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
          mempty = (mempty, mempty, mempty)
          (a1,b1,c1) `mappend` (a2,b2,c2) =
                  (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2)


  instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
          mempty = (mempty, mempty, mempty, mempty)
          (a1,b1,c1,d1) `mappend` (a2,b2,c2,d2) =
                  (a1 `mappend` a2, b1 `mappend` b2,
                   c1 `mappend` c2, d1 `mappend` d2)


  instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
                  Monoid (a,b,c,d,e) where
          mempty = (mempty, mempty, mempty, mempty, mempty)
          (a1,b1,c1,d1,e1) `mappend` (a2,b2,c2,d2,e2) =
                  (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2,
                   d1 `mappend` d2, e1 `mappend` e2)

  -- lexicographical ordering

  instance Monoid Ordering where
          mempty         = EQ
          LT `mappend` _ = LT
          EQ `mappend` y = y
          GT `mappend` _ = GT

  -- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
  -- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
  -- turned into a monoid simply by adjoining an element @e@ not in @S@
  -- and defining @e*e = e@ and @e*s = s = s*e@ for all @s ∈ S@.\" Since
  -- there used to be no \"Semigroup\" typeclass providing just 'mappend',
  -- we use 'Monoid' instead.
  --
  -- @since 2.01
  instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

  -- | For tuples, the 'Monoid' constraint on @a@ determines
  -- how the first values merge.
  -- For example, 'String's concatenate:
  --
  -- > ("hello ", (+15)) <*> ("world!", 2002)
  -- > ("hello world!",2017)
  --
  -- @since 2.01
  instance Monoid a => Applicative ((,) a) where
      pure x = (mempty, x)
      (u, f) <*> (v, x) = (u `mappend` v, f x)
      liftA2 f (u, x) (v, y) = (u `mappend` v, f x y)

  -- | @since 4.9.0.0
  instance Monoid a => Monad ((,) a) where
      (u, a) >>= k = case k a of (v, b) -> (u `mappend` v, b)

  {- | The 'Functor' class is used for types that can be mapped over.
  Instances of 'Functor' should satisfy the following laws:

  > fmap id  ==  id
  > fmap (f . g)  ==  fmap f . fmap g

  The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
  satisfy these laws.
  -}

  -- Need kind signature due to Note [CUSKification] in Promote
  class  Functor (f :: Type -> Type)  where
      fmap        :: (a -> b) -> f a -> f b

      -- | Replace all locations in the input with the same value.
      -- The default definition is @'fmap' . 'const'@, but this may be
      -- overridden with a more efficient version.
      (<$)        :: a -> f b -> f a
      (<$)        =  fmap . const

  class Functor f => Applicative (f :: Type -> Type) where
      -- | Lift a value.
      pure :: a -> f a

      -- | Sequential application.
      --
      -- A few functors support an implementation of '<*>' that is more
      -- efficient than the default one.
      (<*>) :: f (a -> b) -> f a -> f b
      (<*>) = liftA2 id

      -- | Lift a binary function to actions.
      --
      -- Some functors support an implementation of 'liftA2' that is more
      -- efficient than the default one. In particular, if 'fmap' is an
      -- expensive operation, it is likely better to use 'liftA2' than to
      -- 'fmap' over the structure and then use '<*>'.
      liftA2 :: (a -> b -> c) -> f a -> f b -> f c
      liftA2 f x = (<*>) (fmap f x)

      -- | Sequence actions, discarding the value of the first argument.
      (*>) :: f a -> f b -> f b
      a1 *> a2 = (id <$ a1) <*> a2
      -- This is essentially the same as liftA2 (flip const), but if the
      -- Functor instance has an optimized (<$), it may be better to use
      -- that instead. Before liftA2 became a method, this definition
      -- was strictly better, but now it depends on the functor. For a
      -- functor supporting a sharing-enhancing (<$), this definition
      -- may reduce allocation by preventing a1 from ever being fully
      -- realized. In an implementation with a boring (<$) but an optimizing
      -- liftA2, it would likely be better to define (*>) using liftA2.

      -- | Sequence actions, discarding the value of the second argument.
      (<*) :: f a -> f b -> f a
      (<*) = liftA2 const

  -- | A variant of '<*>' with the arguments reversed.
  (<**>) :: Applicative f => f a -> f (a -> b) -> f b
  (<**>) = liftA2 (\a f -> f a)
  -- Don't use $ here, see the note at the top of the page

  -- | Lift a function to actions.
  -- This function may be used as a value for `fmap` in a `Functor` instance.
  liftA :: Applicative f => (a -> b) -> f a -> f b
  liftA f a = pure f <*> a
  -- Caution: since this may be used for `fmap`, we can't use the obvious
  -- definition of liftA = fmap.

  -- | Lift a ternary function to actions.
  liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  liftA3 f a b c = liftA2 f a b <*> c

  -- | The 'join' function is the conventional monad join operator. It
  -- is used to remove one level of monadic structure, projecting its
  -- bound argument into the outer level.
  join              :: (Monad m) => m (m a) -> m a
  join x            =  x >>= id

  class Applicative m => Monad (m :: Type -> Type) where
      -- | Sequentially compose two actions, passing any value produced
      -- by the first as an argument to the second.
      (>>=)       :: forall a b. m a -> (a -> m b) -> m b

      -- | Sequentially compose two actions, discarding any value produced
      -- by the first, like sequencing operators (such as the semicolon)
      -- in imperative languages.
      (>>)        :: forall a b. m a -> m b -> m b
      m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
      {-# INLINE (>>) #-}

      -- | Inject a value into the monadic type.
      return      :: a -> m a
      return      = pure

      -- | Fail with a message.  This operation is not part of the
      -- mathematical definition of a monad, but is invoked on pattern-match
      -- failure in a @do@ expression.
      --
      -- As part of the MonadFail proposal (MFP), this function is moved
      -- to its own class 'MonadFail' (see "Control.Monad.Fail" for more
      -- details). The definition here will be removed in a future
      -- release.
      fail        :: Symbol -> m a
      fail s      = error s

 |])

-- ($) is a special case, because its kind-inference data constructors
-- clash with (:). See #29.
type family (f :: TyFun a b -> *) $ (x :: a) :: b
type instance f $ x = f @@ x
infixr 0 $

data ($$) :: TyFun (TyFun a b -> *) (TyFun a b -> *) -> *
type instance Apply ($$) arg = ($$$) arg

data ($$$) :: (TyFun a b -> *) -> TyFun a b -> *
type instance Apply (($$$) f) arg = ($$$$) f arg

type ($$$$) a b = ($) a b

(%$) :: forall (f :: TyFun a b -> *) (x :: a).
        Sing f -> Sing x -> Sing (($$) @@ f @@ x)
f %$ x = applySing f x
infixr 0 %$

type family (f :: TyFun a b -> *) $! (x :: a) :: b
type instance f $! x = f @@ x
infixr 0 $!

data ($!$) :: TyFun (TyFun a b -> *) (TyFun a b -> *) -> *
type instance Apply ($!$) arg = ($!$$) arg

data ($!$$) :: (TyFun a b -> *) -> TyFun a b -> *
type instance Apply (($!$$) f) arg = ($!$$$) f arg

type ($!$$$) a b = ($!) a b

(%$!) :: forall (f :: TyFun a b -> *) (x :: a).
        Sing f -> Sing x -> Sing (($!$) @@ f @@ x)
f %$! x = applySing f x
infixr 0 %$!
