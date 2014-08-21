{- Data/Singletons/Optimize.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Optimizes the output of TH code generation.
-}

module Data.Singletons.Optimize ( optimize ) where

import Data.Singletons.Names
import Data.Singletons.Util
import Language.Haskell.TH.Syntax

import Control.Monad
import Data.Generics
import Data.Char
import Data.Maybe

-- | Optimize the result of a call to a singletons generation function.
-- For example:
--
-- > $(optimize $ singletons [d| ... |])
--
-- or
--
-- > $(optimize $ promoteEqInstances [...])
--
-- Optimizing may make a substantial difference in compile times. This is
-- kept as a separate function (that is, not done automatically) because it
-- is still experimental.
optimize :: Quasi q => q [Dec] -> q [Dec]
optimize qdecs = do
  decs <- qdecs
  return $ everywhere (mkT flattenApply) decs

flattenApply :: Type -> Type
flattenApply (AppT (ConT app) ty)
  | app == applyName
  , Just (base, n, args) <- splitSymbolApp_maybe ty
  = foldl AppT (ConT $ promoteTySym base (n+1)) args
flattenApply t = t

splitSymbolApp_maybe :: Type -> Maybe (Name, Int, [Type])
splitSymbolApp_maybe = go []
  where
    go args (AppT fun arg) = go (arg:args) fun
    go args (ConT name)
      | Just (base, n) <- splitSymbolName_maybe name
      , length args == n
      = Just (base, n, args)
    go _ _ = Nothing

-- Inverse of promoteTySym
splitSymbolName_maybe :: Name -> Maybe (Name, Int)
splitSymbolName_maybe name
  | isHsLetter (head str)
  = do let (digs, nondigs) = span isDigit rev
       n <- readOne (reverse digs)
       let (mys, rest) = splitAt 3 nondigs
       guard (mys == "myS")
       return (mkName (reverse rest), n)
  | otherwise
  = let (dollars, nondollars) = span (== '$') rev in
    if null nondollars
    then let n = length dollars - 2 in
         guard (n >= 0) >> Just (mkName "$", n)   -- special case for ($)
    else let n = length dollars - 1 in
         guard (n >= 0) >> Just (mkName (reverse nondollars), n)

  where
    str = nameBase name
    rev = reverse str

readOne :: Read a => String -> Maybe a
readOne str = do
  let attempts = reads str
  listToMaybe [ a | (a, leftovers) <- attempts
                  , null leftovers ]
