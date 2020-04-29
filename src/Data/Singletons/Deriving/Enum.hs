-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Enum
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Enum instances
--
----------------------------------------------------------------------------

module Data.Singletons.Deriving.Enum ( mkEnumInstance ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Desugar
import Data.Singletons.Deriving.Util
import Data.Singletons.Syntax
import Data.Singletons.Util
import Data.Singletons.Names
import Control.Monad
import Data.Maybe

-- monadic for failure only
mkEnumInstance :: DsMonad q => DerivDesc q
mkEnumInstance mb_ctxt ty (DataDecl _ _ cons) = do
  -- GHC only allows deriving Enum instances for enumeration types (i.e., those
  -- data types whose constructors all lack fields). We perform the same
  -- validity check here.
  --
  -- GHC actually goes further than we do. GHC will give a specific error
  -- message if you attempt to derive an instance for a "non-vanilla" data
  -- type—that is, a data type that uses features not expressible with
  -- Haskell98 syntax, such as existential quantification. Checking whether
  -- a type variable is existentially quantified is difficult in Template
  -- Haskell, so we omit this check.
  when (null cons ||
        any (\(DCon _ _ _ f _) -> not (null $ tysOfConFields f)) cons) $
    fail ("Can't derive Enum instance for " ++ pprint (typeToTH ty) ++ ".")

  n <- qNewName "n"
  let to_enum = UFunction [DClause [DVarP n] (to_enum_rhs cons [0..])]
      to_enum_rhs [] _ = DVarE errorName `DAppE` DLitE (StringL "toEnum: bad argument")
      to_enum_rhs (DCon _ _ name _ _ : rest) (num:nums) =
        DCaseE (DVarE equalsName `DAppE` DVarE n `DAppE` DLitE (IntegerL num))
          [ DMatch (DConP trueName []) (DConE name)
          , DMatch (DConP falseName []) (to_enum_rhs rest nums) ]
      to_enum_rhs _ _ = error "Internal error: exhausted infinite list in to_enum_rhs"

      from_enum = UFunction (zipWith (\i con -> DClause [DConP (extractName con) []]
                                                        (DLitE (IntegerL i)))
                                     [0..] cons)
  return (InstDecl { id_cxt     = fromMaybe [] mb_ctxt
                   , id_name    = enumName
                   , id_arg_tys = [ty]
                   , id_sigs    = mempty
                   , id_meths   = [ (toEnumName, to_enum)
                                  , (fromEnumName, from_enum) ] })
