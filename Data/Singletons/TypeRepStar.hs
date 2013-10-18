{- Data/Singletons/TypeRepStar.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains the definitions for considering TypeRep to be the demotion
of *. This is still highly experimental, so expect unusual results!

-}

{-# LANGUAGE RankNTypes, TypeFamilies, KindSignatures, FlexibleInstances,
             GADTs, UndecidableInstances, ScopedTypeVariables, DataKinds,
             MagicHash, CPP #-}

module Data.Singletons.TypeRepStar ( Sing(STypeRep) ) where

import Data.Singletons.Core
import Data.Singletons.Types
import Data.Typeable
import GHC.Exts
import Unsafe.Coerce

data instance Sing (a :: *) where
  STypeRep :: Typeable a => Sing a

instance Typeable a => SingI (a :: *) where
  sing = STypeRep
instance SingKind ('KProxy :: KProxy *) where
  type DemoteRep ('KProxy :: KProxy *) = TypeRep
  fromSing (STypeRep :: Sing a) = typeOf (undefined :: a)
  toSing = dirty_mk_STypeRep

newtype DI = Don'tInstantiate (Typeable a => Sing a)
dirty_mk_STypeRep :: TypeRep -> SomeSing ('KProxy :: KProxy *)
dirty_mk_STypeRep rep =
#if __GLASGOW_HASKELL__ >= 707
  let justLikeTypeable :: Proxy# a -> TypeRep
      justLikeTypeable _ = rep
  in
#else
  let justLikeTypeable :: a -> TypeRep
      justLikeTypeable _ = rep
  in
#endif
  unsafeCoerce (Don'tInstantiate STypeRep) justLikeTypeable
