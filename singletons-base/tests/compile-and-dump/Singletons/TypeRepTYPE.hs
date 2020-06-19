{-# LANGUAGE MagicHash #-}
module TypeRepTYPE where

import Data.Kind (Type)
import Data.Singletons.Base.TypeRepTYPE
import Data.Singletons.Decide
import GHC.Exts (Char#, RuntimeRep(..), TYPE, Word#)
import Prelude.Singletons
import Type.Reflection (Typeable, typeRep)

eqTYPETest1 :: (Type == Type) :~: 'True
eqTYPETest1 = Refl

eqTYPETest2 :: (Type == TYPE 'IntRep) :~: 'False
eqTYPETest2 = Refl

f :: Sing (a :: Type) -> Maybe a
f tr
  | Proved Refl <- tr %~ sing @Bool
  = Just True
  | Proved Refl <- tr %~ sing @Ordering
  = Just EQ
  | otherwise
  = Nothing

data MaybeWordRep (a :: TYPE 'WordRep)
  = NothingWordRep
  | JustWordRep a

g :: Sing (a :: TYPE 'WordRep) -> MaybeWordRep a
g tr
  | Proved Refl <- tr %~ sing @Word#
  = JustWordRep 42##
  | Proved Refl <- tr %~ sing @Char#
  = JustWordRep 'j'#
  | otherwise
  = NothingWordRep

h :: forall (rep :: RuntimeRep) (a :: TYPE rep). Typeable a => Sing a
h = typeRep @a
