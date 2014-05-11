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
mkBoundedTypeInstance kind@(DConK name _) cons = do
  -- We can derive instance of Bounded if datatype is an enumeration (all
  -- constructors must be nullary) or has only one constructor. See Section 11
  -- of Haskell 2010 Language Report.
  -- Note that order of conditions below is important.
  when (null cons
       || (any (\(DCon _ _ _ f) -> not . null . tysOfConFields $ f) cons
            && (not . null . tail $ cons))) $
       fail ("Can't derive promoted Bounded instance for " ++ show name
             ++ " datatype.")
  -- at this point we know that either we have a datatype that has only one
  -- constructor or a datatype where each constructor is nullary
  let (DCon _ _ minName fields) = head cons
      (DCon _ _ maxName _)      = last cons
      pbounded_name = promoteClassName boundedName
      fieldsCount   = length $ tysOfConFields fields
      (minRHS, maxRHS) = case fieldsCount of
        0 -> (DConT minName, DConT maxName)
        _ ->
          let minEqnRHS = foldType (DConT minName)
                                   (replicate fieldsCount (DConT tyminBoundName))
              maxEqnRHS = foldType (DConT maxName)
                                   (replicate fieldsCount (DConT tymaxBoundName))
          in (minEqnRHS, maxEqnRHS)
  return $ [ DInstanceD [] (DConT pbounded_name `DAppT` kindParam kind)
             [ DTySynInstD tyminBoundName (DTySynEqn [] minRHS)
             , DTySynInstD tymaxBoundName (DTySynEqn [] maxRHS)
             ]
           ]
mkBoundedTypeInstance _ _ = fail "Error deriving Bounded instance"
