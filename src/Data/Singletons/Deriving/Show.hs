-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Show
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Show instances
--
----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Singletons.Deriving.Show (
    mkShowInstance
  , mkShowSingContext
  ) where

import Language.Haskell.TH.Syntax hiding (showName)
import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Util
import Data.Singletons.Syntax
import Data.Singletons.Deriving.Infer
import Data.Singletons.Deriving.Util
import Data.Maybe (fromMaybe)
import GHC.Lexeme (startsConSym, startsVarSym)
import GHC.Show (appPrec, appPrec1)

mkShowInstance :: DsMonad q => DerivDesc q
mkShowInstance mb_ctxt ty (DataDecl _ _ cons) = do
  clauses <- mk_showsPrec cons
  constraints <- inferConstraintsDef mb_ctxt (DConPr showName) ty cons
  return $ InstDecl { id_cxt = constraints
                    , id_name = showName
                    , id_arg_tys = [ty]
                    , id_meths = [ (showsPrecName, UFunction clauses) ] }

mk_showsPrec :: DsMonad q => [DCon] -> q [DClause]
mk_showsPrec cons = do
    p <- newUniqueName "p" -- The precedence argument (not always used)
    if null cons
       then do v <- newUniqueName "v"
               pure [DClause [DWildPa, DVarPa v] (DCaseE (DVarE v) [])]
       else mapM (mk_showsPrec_clause p) cons

mk_showsPrec_clause :: forall q. DsMonad q
                    => Name -> DCon
                    -> q DClause
mk_showsPrec_clause p (DCon _ _ con_name con_fields _) = go con_fields
  where
    go :: DConFields -> q DClause

    -- No fields: print just the constructor name, with no parentheses
    go (DNormalC _ []) = return $
      DClause [DWildPa, DConPa con_name []] $
        DVarE showStringName `DAppE` dStringE (parenInfixConName con_name "")

    -- Infix constructors have special Show treatment.
    go (DNormalC True [_, _]) = do
      argL <- newUniqueName "argL"
      argR <- newUniqueName "argR"
      fi <- fromMaybe defaultFixity <$> reifyFixityWithLocals con_name
      let con_prec = case fi of Fixity prec _ -> prec
          op_name  = nameBase con_name
          infixOpE = DAppE (DVarE showStringName) . dStringE $
                       if isInfixDataCon op_name
                          then " "  ++ op_name ++ " "
                          -- Make sure to handle infix data constructors
                          -- like (Int `Foo` Int)
                          else " `" ++ op_name ++ "` "
      return $ DClause [DVarPa p, DConPa con_name [DVarPa argL, DVarPa argR]] $
        (DVarE showParenName `DAppE` (DVarE gtName `DAppE` DVarE p
                                                   `DAppE` dIntegerE con_prec))
          `DAppE` (DVarE composeName
                     `DAppE` showsPrecE (con_prec + 1) argL
                     `DAppE` (DVarE composeName
                                `DAppE` infixOpE
                                `DAppE` showsPrecE (con_prec + 1) argR))

    go (DNormalC _ tys) = do
      args <- mapM (const $ newUniqueName "arg") tys
      let show_args     = map (showsPrecE appPrec1) args
          composed_args = foldr1 (\v q -> DVarE composeName
                                           `DAppE` v
                                           `DAppE` (DVarE composeName
                                                     `DAppE` DVarE showSpaceName
                                                     `DAppE` q)) show_args
          named_args = DVarE composeName
                         `DAppE` (DVarE showStringName
                                   `DAppE` dStringE (parenInfixConName con_name " "))
                         `DAppE` composed_args
      return $ DClause [DVarPa p, DConPa con_name $ map DVarPa args] $
        DVarE showParenName
          `DAppE` (DVarE gtName `DAppE` DVarE p `DAppE` dIntegerE appPrec)
          `DAppE` named_args

    -- We show a record constructor with no fields the same way we'd show a
    -- normal constructor with no fields.
    go (DRecC []) = go (DNormalC False [])

    go (DRecC tys) = do
      args <- mapM (const $ newUniqueName "arg") tys
      let show_args =
            concatMap (\((arg_name, _, _), arg) ->
                        let arg_nameBase = nameBase arg_name
                            infix_rec    = showParen (isSym arg_nameBase)
                                                     (showString arg_nameBase) ""
                        in [ DVarE showStringName `DAppE` dStringE (infix_rec ++ " = ")
                           , showsPrecE 0 arg
                           , DVarE showCommaSpaceName
                           ])
                      (zip tys args)
          brace_comma_args =   (DVarE showCharName `DAppE` dCharE '{')
                             : take (length show_args - 1) show_args
          composed_args = foldr (\x y -> DVarE composeName `DAppE` x `DAppE` y)
                                (DVarE showCharName `DAppE` dCharE '}')
                                brace_comma_args
          named_args = DVarE composeName
                         `DAppE` (DVarE showStringName
                                   `DAppE` dStringE (parenInfixConName con_name " "))
                         `DAppE` composed_args
      return $ DClause [DVarPa p, DConPa con_name $ map DVarPa args] $
        DVarE showParenName
          `DAppE` (DVarE gtName `DAppE` DVarE p `DAppE` dIntegerE appPrec)
          `DAppE` named_args

-- | Parenthesize an infix constructor name if it is being applied as a prefix
-- function (e.g., data Amp a = (:&) a a)
parenInfixConName :: Name -> ShowS
parenInfixConName conName =
    let conNameBase = nameBase conName
    in showParen (isInfixDataCon conNameBase) $ showString conNameBase

showsPrecE :: Int -> Name -> DExp
showsPrecE prec n = DVarE showsPrecName `DAppE` dIntegerE prec `DAppE` DVarE n

dCharE :: Char -> DExp
dCharE c = DLitE $ StringL [c] -- There aren't type-level characters yet,
                               -- so fake it with a string

dStringE :: String -> DExp
dStringE = DLitE . StringL

dIntegerE :: Int -> DExp
dIntegerE = DLitE . IntegerL . fromIntegral

isSym :: String -> Bool
isSym ""      = False
isSym (c : _) = startsVarSym c || startsConSym c

-- | Turn a context like @('Show' a, 'Show' b)@ into @('ShowSing' a, 'ShowSing' b)@.
-- This is necessary for standalone-derived 'Show' instances for singleton types.
mkShowSingContext :: DCxt -> DCxt
mkShowSingContext = map show_to_SingShow
  where
    show_to_SingShow :: DPred -> DPred
    show_to_SingShow = modifyConNameDPred $ \n ->
                         if n == showName
                            then showSingName
                            else n
