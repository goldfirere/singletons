module Singletons.ShowDeriving where

import Data.Type.Equality
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Show
import Data.Singletons.TH

$(singletons [d|
    data Foo1 = MkFoo1 deriving Show

    data Foo2 a = MkFoo2a a a
                | a `MkFoo2b` a
                | (:*:) a a
                | a :&: a
                deriving Show

    data Foo3 = MkFoo3 { getFoo3a :: Bool, (***) :: Bool } deriving Show

    |])

foo1 :: "MkFoo1" :~: Show_ MkFoo1
foo1 = Refl

foo2a :: "(MkFoo2a LT GT)" :~: ShowsPrec 11 (MkFoo2a LT GT) ""
foo2a = Refl

foo2b :: "True `MkFoo2b` False" :~: Show_ (True `MkFoo2b` False)
foo2b = Refl

foo2c :: "(:*:) () ()" :~: Show_ ('() :*: '())
foo2c = Refl

foo2d :: "(False :&: True)" :~: ShowsPrec 10 (False :&: True) ""
foo2d = Refl

foo3 :: "MkFoo3 {getFoo3a = True, (***) = False}" :~: Show_ (MkFoo3 True False)
foo3 = Refl
