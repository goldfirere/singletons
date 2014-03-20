{- ByHandAux.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

This file just defines some Template Haskell to making dealing with the
lack of empty case in GHC 7.6.3 palatable.

-}

{-# LANGUAGE TemplateHaskell, CPP, LambdaCase #-}

module Test.ByHandAux where

import Language.Haskell.TH

emptyLamCase :: Q Exp
emptyLamCase = [| \case _ -> error "should be empty case" |]
