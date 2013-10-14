{- Data/Singletons/TypeRepStar.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains the definitions for considering TypeRep to be the demotion
of *. This is still highly experimental, so expect unusual results!

-}

{-# LANGUAGE RankNTypes, TypeFamilies, KindSignatures, FlexibleInstances,
             GADTs, UndecidableInstances, ScopedTypeVariables, DataKinds #-}

module Data.Singletons.TypeRepStar where

import Data.Singletons
import Data.Typeable

data instance Sing (a :: *) where
  STypeRep :: Typeable a => Sing a

sTypeRep :: forall (a :: *). Typeable a => Sing a
sTypeRep = STypeRep

instance Typeable a => SingI (a :: *) where
  sing = STypeRep
instance SingE ('KProxy :: KProxy *) where
  type DemoteRep ('KProxy :: KProxy *) = TypeRep
  fromSing (STypeRep :: Sing a) = typeOf (undefined :: a)
instance SingKind ('KProxy :: KProxy *) where
  singInstance STypeRep = SingInstance
