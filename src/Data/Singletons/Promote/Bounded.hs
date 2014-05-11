{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Promote.Bounded
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of promoted Bounded instances
--
----------------------------------------------------------------------------

module Data.Singletons.Promote.Bounded where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Util
import Control.Monad

mkBoundedTypeInstance :: Quasi q => DKind -> [DCon] -> q [DDec]
mkBoundedTypeInstance kind@(DConK name tyvars) cons = do
  -- See Section 11 of Haskell 2010 Language Report
  -- for rationale behind this check.
  -- Note that order of these conditions is important.
  when (null cons || (not (null tyvars) && (not . null . tail $ cons))) $
       fail ("Can't derive promoted Bounded instance for " ++ show name
             ++ " datatype.")
  let (DCon _ _ minName fields) = head cons
      (DCon _ _ maxName _)      = last cons
      pbounded_name   = promoteClassName boundedName
      (ctx, minRHS, maxRHS) = case tyvars of
        []    -> ([], DConT minName, DConT maxName)
        (_:_) ->
          let fieldsCount = length $ tysOfConFields fields
              minEqnRHS = foldType (DConT minName)
                                   (replicate fieldsCount (DConT tyminBoundName))
              maxEqnRHS = foldType (DConT maxName)
                                   (replicate fieldsCount (DConT tymaxBoundName))
          in ([], minEqnRHS, maxEqnRHS)
  return $ [ DInstanceD ctx (DConT pbounded_name `DAppT` kindParam kind)
             [ DTySynInstD tyminBoundName (DTySynEqn [] minRHS)
             , DTySynInstD tymaxBoundName (DTySynEqn [] maxRHS)
             ]
           ]
mkBoundedTypeInstance _ _ = fail "Error deriving Bounded instance"
