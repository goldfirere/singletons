{-# LANGUAGE ConstraintKinds #-}

module Singletons.Names where

import Data.Singletons.Prelude
import Language.Haskell.TH
import Data.Singletons.Names

-- this tests that all the wired-in names work
type A y z = $(return $ ConT tyCompareName) y z
type B = $(return $ ConT tyminBoundName)
type C = $(return $ ConT tymaxBoundName)
type D y z = $(return $ ConT tyEqName) y z
type E = $(return $ ConT ordLTSymName)
type F = $(return $ ConT ordEQSymName)
type G = $(return $ ConT ordGTSymName)
type H = $(return $ ConT sEqClassName)
i = $(return $ VarE sEqMethName) STuple0 STuple0
j = $(return $ VarE sIfName)
k = $(return $ ConE sconsName)
l = $(return $ ConE snilName)
type M y = $(return $ ConT sListName) y
type N y z = $(return $ ConT tyThenCmpName) y z
type O y = $(return $ ConT tyFromIntegerName) y
type P y = $(return $ ConT tyNegateName) y
