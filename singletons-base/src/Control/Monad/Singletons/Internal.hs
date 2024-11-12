{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Singletons.Internal
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of:
--
-- * 'Functor'
-- * 'Applicative'
-- * 'Alternative'
-- * 'Monad'
-- * 'MonadPlus'
--
-- As well as auxiliary definitions.
--
-- This module exists to break up import cycles.
--
----------------------------------------------------------------------------

module Control.Monad.Singletons.Internal where

import Control.Applicative
import Control.Monad
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import Data.Singletons.Base.Instances
import Data.Singletons.TH
import GHC.Base.Singletons

$(singletonsOnly [d|
  infixl 4  <$

  {- -| The 'Functor' class is used for types that can be mapped over.
  Instances of 'Functor' should satisfy the following laws:

  > fmap id  ==  id
  > fmap (f . g)  ==  fmap f . fmap g

  The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
  satisfy these laws.
  -}

  -- See Note [Using standalone kind signatures not present in the base library]
  type   Functor :: (Type -> Type) -> Constraint
  class  Functor f  where
      fmap        :: (a -> b) -> f a -> f b

      -- -| Replace all locations in the input with the same value.
      -- The default definition is @'fmap' . 'const'@, but this may be
      -- overridden with a more efficient version.
      (<$)        :: a -> f b -> f a
      (<$)        =  fmap . const

  infixl 4 <*>, <*, *>, <**>

  -- -| A functor with application, providing operations to
  --
  -- -* embed pure expressions ('pure'), and
  --
  -- -* sequence computations and combine their results ('<*>' and 'liftA2').
  --
  -- A minimal complete definition must include implementations of 'pure'
  -- and of either '<*>' or 'liftA2'. If it defines both, then they must behave
  -- the same as their default definitions:
  --
  --      @('<*>') = 'liftA2' 'id'@
  --
  --      @'liftA2' f x y = f '<$>' x '<*>' y@
  --
  -- Further, any definition must satisfy the following:
  --
  -- [/identity/]
  --
  --      @'pure' 'id' '<*>' v = v@
  --
  -- [/composition/]
  --
  --      @'pure' (.) '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
  --
  -- [/homomorphism/]
  --
  --      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
  --
  -- [/interchange/]
  --
  --      @u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@
  --
  --
  -- The other methods have the following default definitions, which may
  -- be overridden with equivalent specialized implementations:
  --
  --   * @u '*>' v = ('id' '<$' u) '<*>' v@
  --
  --   * @u '<*' v = 'liftA2' 'const' u v@
  --
  -- As a consequence of these laws, the 'Functor' instance for @f@ will satisfy
  --
  --   * @'fmap' f x = 'pure' f '<*>' x@
  --
  --
  -- It may be useful to note that supposing
  --
  --      @forall x y. p (q x y) = f x . g y@
  --
  -- it follows from the above that
  --
  --      @'liftA2' p ('liftA2' q u v) = 'liftA2' f u . 'liftA2' g v@
  --
  --
  -- If @f@ is also a 'Monad', it should satisfy
  --
  --   * @'pure' = 'return'@
  --
  --   * @('<*>') = 'ap'@
  --
  --   * @('*>') = ('>>')@
  --
  -- (which implies that 'pure' and '<*>' satisfy the applicative functor laws).

  -- See Note [Using standalone kind signatures not present in the base library]
  type Applicative :: (Type -> Type) -> Constraint
  class Functor f => Applicative f where
      -- {-# MINIMAL pure, ((<*>) | liftA2) #-}
      -- -| Lift a value.
      pure :: a -> f a

      -- -| Sequential application.
      --
      -- A few functors support an implementation of '<*>' that is more
      -- efficient than the default one.
      (<*>) :: f (a -> b) -> f a -> f b
      (<*>) = liftA2 id

      -- -| Lift a binary function to actions.
      --
      -- Some functors support an implementation of 'liftA2' that is more
      -- efficient than the default one. In particular, if 'fmap' is an
      -- expensive operation, it is likely better to use 'liftA2' than to
      -- 'fmap' over the structure and then use '<*>'.
      liftA2 :: (a -> b -> c) -> f a -> f b -> f c
      liftA2 f x = (<*>) (fmap f x)

      -- -| Sequence actions, discarding the value of the first argument.
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

      -- -| Sequence actions, discarding the value of the second argument.
      (<*) :: f a -> f b -> f a
      (<*) = liftA2 const

  -- -| A variant of '<*>' with the arguments reversed.
  (<**>) :: Applicative f => f a -> f (a -> b) -> f b
  (<**>) = liftA2 (\a f -> f a)
  -- Don't use $ here, see the note at the top of the page

  -- -| Lift a function to actions.
  -- This function may be used as a value for `fmap` in a `Functor` instance.
  liftA :: Applicative f => (a -> b) -> f a -> f b
  liftA f a = pure f <*> a
  -- Caution: since this may be used for `fmap`, we can't use the obvious
  -- definition of liftA = fmap.

  -- -| Lift a ternary function to actions.
  liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  liftA3 f a b c = liftA2 f a b <*> c

  infixl 1  >>, >>=
  infixr 1  =<<

  -- -| The 'join' function is the conventional monad join operator. It
  -- is used to remove one level of monadic structure, projecting its
  -- bound argument into the outer level.
  --
  -- ==== __Examples__
  --
  -- A common use of 'join' is to run an 'IO' computation returned from
  -- an 'GHC.Conc.STM' transaction, since 'GHC.Conc.STM' transactions
  -- can't perform 'IO' directly. Recall that
  --
  -- @
  -- 'GHC.Conc.atomically' :: STM a -> IO a
  -- @
  --
  -- is used to run 'GHC.Conc.STM' transactions atomically. So, by
  -- specializing the types of 'GHC.Conc.atomically' and 'join' to
  --
  -- @
  -- 'GHC.Conc.atomically' :: STM (IO b) -> IO (IO b)
  -- 'join'       :: IO (IO b)  -> IO b
  -- @
  --
  -- we can compose them as
  --
  -- @
  -- 'join' . 'GHC.Conc.atomically' :: STM (IO b) -> IO b
  -- @
  --
  -- to run an 'GHC.Conc.STM' transaction and the 'IO' action it
  -- returns.
  join              :: (Monad m) => m (m a) -> m a
  join x            =  x >>= id

  {- -| The 'Monad' class defines the basic operations over a /monad/,
  a concept from a branch of mathematics known as /category theory/.
  From the perspective of a Haskell programmer, however, it is best to
  think of a monad as an /abstract datatype/ of actions.
  Haskell's @do@ expressions provide a convenient syntax for writing
  monadic expressions.

  Instances of 'Monad' should satisfy the following laws:

  * @'return' a '>>=' k  =  k a@
  * @m '>>=' 'return'  =  m@
  * @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@

  Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:

  * @'pure' = 'return'@
  * @('<*>') = 'ap'@

  The above laws imply:

  * @'fmap' f xs  =  xs '>>=' 'return' . f@
  * @('>>') = ('*>')@

  and that 'pure' and ('<*>') satisfy the applicative functor laws.

  The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
  defined in the "Prelude" satisfy these laws.
  -}

  -- See Note [Using standalone kind signatures not present in the base library]
  type Monad :: (Type -> Type) -> Constraint
  class Applicative m => Monad m where
      -- -| Sequentially compose two actions, passing any value produced
      -- by the first as an argument to the second.
      (>>=)       :: forall a b. m a -> (a -> m b) -> m b

      -- -| Sequentially compose two actions, discarding any value produced
      -- by the first, like sequencing operators (such as the semicolon)
      -- in imperative languages.
      (>>)        :: forall a b. m a -> m b -> m b
      m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]

      -- -| Inject a value into the monadic type.
      return      :: a -> m a
      return      = pure

  {- Note [Recursive bindings for Applicative/Monad]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The original Applicative/Monad proposal stated that after
  implementation, the designated implementation of (>>) would become

    (>>) :: forall a b. m a -> m b -> m b
    (>>) = (*>)

  by default. You might be inclined to change this to reflect the stated
  proposal, but you really shouldn't! Why? Because people tend to define
  such instances the /other/ way around: in particular, it is perfectly
  legitimate to define an instance of Applicative (*>) in terms of (>>),
  which would lead to an infinite loop for the default implementation of
  Monad! And people do this in the wild.

  This turned into a nasty bug that was tricky to track down, and rather
  than eliminate it everywhere upstream, it's easier to just retain the
  original default.

  -}

  -- -| Same as '>>=', but with the arguments interchanged.
  (=<<)           :: Monad m => (a -> m b) -> m a -> m b
  f =<< x         = x >>= f

  -- -| Conditional execution of 'Applicative' expressions. For example,
  --
  -- > when debug (putStrLn "Debugging")
  --
  -- will output the string @Debugging@ if the Boolean value @debug@
  -- is 'True', and otherwise do nothing.
  when      :: (Applicative f) => Bool -> f () -> f ()
  when p s  = if p then s else pure ()

  -- -| Promote a function to a monad.
  liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
  liftM f m1              = do { x1 <- m1; return (f x1) }

  -- -| Promote a function to a monad, scanning the monadic arguments from
  -- left to right.  For example,
  --
  -- > liftM2 (+) [0,1] [0,2] = [0,2,1,3]
  -- > liftM2 (+) (Just 1) Nothing = Nothing
  --
  liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
  liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
  -- Caution: since this may be used for `liftA2`, we can't use the obvious
  -- definition of liftM2 = liftA2.

  -- -| Promote a function to a monad, scanning the monadic arguments from
  -- left to right (cf. 'liftM2').
  liftM3  :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
  liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

  -- -| Promote a function to a monad, scanning the monadic arguments from
  -- left to right (cf. 'liftM2').
  liftM4  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
  liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }

  -- -| Promote a function to a monad, scanning the monadic arguments from
  -- left to right (cf. 'liftM2').
  liftM5  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
  liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

  {- -| In many situations, the 'liftM' operations can be replaced by uses of
  'ap', which promotes function application.

  > return f `ap` x1 `ap` ... `ap` xn

  is equivalent to

  > liftMn f x1 x2 ... xn

  -}

  ap                :: (Monad m) => m (a -> b) -> m a -> m b
  ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
  -- Since many Applicative instances define (<*>) = ap, we
  -- cannot define ap = (<*>)

  -- -----------------------------------------------------------------------------
  -- The Alternative class definition

  infixl 3 <|>

  -- -| A monoid on applicative functors.
  --
  -- If defined, 'some' and 'many' should be the least solutions
  -- of the equations:
  --
  -- -* @'some' v = (:) '<$>' v '<*>' 'many' v@
  --
  -- -* @'many' v = 'some' v '<|>' 'pure' []@

  -- See Note [Using standalone kind signatures not present in the base library]
  type Alternative :: (Type -> Type) -> Constraint
  class Applicative f => Alternative f where
      -- -| The identity of '<|>'
      empty :: f a
      -- -| An associative binary operation
      (<|>) :: f a -> f a -> f a

      {-
      some and many rely on infinite lists

      -- -| One or more.
      some :: f a -> f [a]
      some v = some_v
        where
          many_v = some_v <|> pure []
          some_v = liftA2 (:) v many_v

      -- -| Zero or more.
      many :: f a -> f [a]
      many v = many_v
        where
          many_v = some_v <|> pure []
          some_v = liftA2 (:) v many_v
      -}

  -- -| @'guard' b@ is @'pure' ()@ if @b@ is 'True',
  -- and 'empty' if @b@ is 'False'.
  guard           :: (Alternative f) => Bool -> f ()
  guard True      =  pure ()
  guard False     =  empty

  -- -----------------------------------------------------------------------------
  -- The MonadPlus class definition

  -- -| Monads that also support choice and failure.

  -- See Note [Using standalone kind signatures not present in the base library]
  type MonadPlus :: (Type -> Type) -> Constraint
  class (Alternative m, Monad m) => MonadPlus m where
     -- -| The identity of 'mplus'.  It should also satisfy the equations
     --
     -- > mzero >>= f  =  mzero
     -- > v >> mzero   =  mzero
     --
     -- The default definition is
     --
     -- @
     -- mzero = 'empty'
     -- @
     mzero :: m a
     mzero = empty

     -- -| An associative operation. The default definition is
     --
     -- @
     -- mplus = ('<|>')
     -- @
     mplus :: m a -> m a -> m a
     mplus = (<|>)
  |])

$(singletonsOnly [d|
  -------------------------------------------------------------------------------
  -- Instances

  deriving instance Functor Maybe
  deriving instance Functor NonEmpty
  deriving instance Functor []
  deriving instance Functor (Either a)

  instance Applicative Maybe where
      pure = Just

      Just f  <*> m       = fmap f m
      Nothing <*> _m      = Nothing

      liftA2 f (Just x) (Just y) = Just (f x y)
      liftA2 _ Just{}   Nothing  = Nothing
      liftA2 _ Nothing  Just{}   = Nothing
      liftA2 _ Nothing  Nothing  = Nothing

      Just _m1 *> m2      = m2
      Nothing  *> _m2     = Nothing

  instance Applicative NonEmpty where
    pure a = a :| []
    (<*>) = ap
    liftA2 = liftM2

  instance Applicative [] where
      pure x = [x]
      (<*>)  = ap
      liftA2 = liftM2
      m *> k = m >>= \_ -> k

  instance Applicative (Either e) where
      pure          = Right
      Left  e <*> _ = Left e
      Right f <*> r = fmap f r

  instance  Monad Maybe  where
      (Just x) >>= k      = k x
      Nothing  >>= _      = Nothing

      (>>) = (*>)

  instance Monad NonEmpty where
    (a :| as) >>= f = b :| (bs ++ bs')
      where b :| bs = f a
            bs' = as >>= toList . f
            toList (c :| cs) = c : cs

  instance Monad []  where
      xs >>= f = foldr ((++) . f) [] xs

  instance Monad (Either e) where
      Left  l >>= _ = Left l
      Right r >>= k = k r

  instance Alternative Maybe where
      empty = Nothing
      Nothing    <|> r = r
      l@(Just{}) <|> _ = l

  instance Alternative [] where
      empty = []
      (<|>) = (++)

  instance MonadPlus Maybe
  instance MonadPlus []
  |])

{-
Note [Using standalone kind signatures not present in the base library]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Various type class definitions in singletons-base (Functor, Foldable,
Alternative, etc.) are defined using standalone kind signatures. These
standalone kind signatures are /not/ present in the original `base` library,
however: these are specifically required by singletons-th. More precisely, all
of these classes are parameterized by a type variable of kind `Type -> Type`,
and we want to ensure that the promoted class (and the defunctionalization
symbols for its class methods) all use `Type -> Type` in their kinds as well.
For more details on why singletons-th requires this, see Note [Propagating kind
information from class standalone kind signatures] in D.S.TH.Promote in
singletons-th.
-}
