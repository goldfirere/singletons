{- ByHandAux.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

This file just defines some Template Haskell to making dealing with the
lack of empty case in GHC 7.6.3 palatable.

-}

{-# LANGUAGE TemplateHaskell, CPP, LambdaCase #-}

#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE EmptyCase #-}
#endif

module Test.ByHandAux where

import Language.Haskell.TH

emptyLamCase :: Q Exp
#if __GLASGOW_HASKELL__ >= 707
emptyLamCase = [| \case {} |]
#else
emptyLamCase = [| \case _ -> error "should be empty case" |]
#endif
