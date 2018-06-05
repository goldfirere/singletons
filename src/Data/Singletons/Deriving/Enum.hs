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
mkEnumInstance mb_ctxt ty (DataDecl data_name tvbs cons) = do
  let data_ty = foldTypeTvbs (DConT data_name) tvbs
  non_vanilla <- isNonVanillaDataType data_ty cons
  when (null cons ||
        any (\(DCon _ _ _ f _) ->
              non_vanilla || not (null $ tysOfConFields f)) cons) $
    fail ("Can't derive Enum instance for " ++ pprint (typeToTH ty) ++ ".")
  n <- qNewName "n"
  let to_enum = UFunction [DClause [DVarPa n] (to_enum_rhs cons [0..])]
      to_enum_rhs [] _ = DVarE errorName `DAppE` DLitE (StringL "toEnum: bad argument")
      to_enum_rhs (DCon _ _ name _ _ : rest) (num:nums) =
        DCaseE (DVarE equalsName `DAppE` DVarE n `DAppE` DLitE (IntegerL num))
          [ DMatch (DConPa trueName []) (DConE name)
          , DMatch (DConPa falseName []) (to_enum_rhs rest nums) ]
      to_enum_rhs _ _ = error "Internal error: exhausted infinite list in to_enum_rhs"

      from_enum = UFunction (zipWith (\i con -> DClause [DConPa (extractName con) []]
                                                        (DLitE (IntegerL i)))
                                     [0..] cons)
  return (InstDecl { id_cxt     = fromMaybe [] mb_ctxt
                   , id_name    = singletonsEnumName
                      -- need to use singletons's Enum class to get the types
                      -- to use Nat instead of Int

                   , id_arg_tys = [ty]
                   , id_meths   = [ (singletonsToEnumName, to_enum)
                                  , (singletonsFromEnumName, from_enum) ] })
