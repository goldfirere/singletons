-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.Deriving.Show
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
module Data.Singletons.TH.Deriving.Show (
    mkShowInstance
  , ShowMode(..)
  , mkShowSingContext
  ) where

import Language.Haskell.TH.Syntax hiding (showName)
import Language.Haskell.TH.Desugar
import Data.Singletons.TH.Deriving.Infer
import Data.Singletons.TH.Deriving.Util
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Syntax
import Data.Singletons.TH.Util
import Data.Maybe (fromMaybe)
import GHC.Lexeme (startsConSym, startsVarSym)
import GHC.Show (appPrec, appPrec1)

mkShowInstance :: OptionsMonad q => ShowMode -> DerivDesc q
mkShowInstance mode mb_ctxt ty (DataDecl _ _ cons) = do
  clauses <- mk_showsPrec mode cons
  constraints <- inferConstraintsDef (fmap (mkShowSingContext mode) mb_ctxt)
                                     (DConT (mk_Show_name mode))
                                     ty cons
  ty' <- mk_Show_inst_ty mode ty
  return $ InstDecl { id_cxt = constraints
                    , id_name = showName
                    , id_arg_tys = [ty']
                    , id_sigs  = mempty
                    , id_meths = [ (showsPrecName, UFunction clauses) ] }

mk_showsPrec :: OptionsMonad q => ShowMode -> [DCon] -> q [DClause]
mk_showsPrec mode cons = do
    p <- newUniqueName "p" -- The precedence argument (not always used)
    if null cons
       then do v <- newUniqueName "v"
               pure [DClause [DWildP, DVarP v] (DCaseE (DVarE v) [])]
       else mapM (mk_showsPrec_clause mode p) cons

mk_showsPrec_clause :: forall q. OptionsMonad q
                    => ShowMode -> Name -> DCon
                    -> q DClause
mk_showsPrec_clause mode p (DCon _ _ con_name con_fields _) = go con_fields
  where
    go :: DConFields -> q DClause
    go con_fields' = do
      opts <- getOptions

      let con_name' :: Name
          con_name' = case mode of
                        ForPromotion  -> con_name
                        ForShowSing{} -> singledDataConName opts con_name

      case con_fields' of

        -- No fields: print just the constructor name, with no parentheses
        DNormalC _ [] -> return $
          DClause [DWildP, DConP con_name' []] $
            DVarE showStringName `DAppE` dStringE (parenInfixConName con_name' "")

        -- Infix constructors have special Show treatment.
        DNormalC True tys@[_, _]
            -- Although the (:) constructor is infix, its singled counterpart SCons
            -- is not, which matters if we're deriving a ShowSing instance.
            -- Unless we remove this special case (see #234), we will simply
            -- shunt it along as if we were dealing with a prefix constructor.
          |  ForShowSing{} <- mode
          ,  con_name == consName
          -> go (DNormalC False tys)

          |  otherwise
          -> do argL   <- newUniqueName "argL"
                argR   <- newUniqueName "argR"
                argTyL <- newUniqueName "argTyL"
                argTyR <- newUniqueName "argTyR"
                fi <- fromMaybe defaultFixity <$> reifyFixityWithLocals con_name'
                let con_prec = case fi of Fixity prec _ -> prec
                    op_name  = nameBase con_name'
                    infixOpE = DAppE (DVarE showStringName) . dStringE $
                                 if isInfixDataCon op_name
                                    then " "  ++ op_name ++ " "
                                    -- Make sure to handle infix data constructors
                                    -- like (Int `Foo` Int)
                                    else " `" ++ op_name ++ "` "
                return $ DClause [ DVarP p
                                 , DConP con_name' $
                                   zipWith (mk_Show_arg_pat mode) [argL, argR] [argTyL, argTyR]
                                 ] $
                  mk_Show_rhs_sig mode [argTyL, argTyR] $
                  (DVarE showParenName `DAppE` (DVarE gtName `DAppE` DVarE p
                                                             `DAppE` dIntegerE con_prec))
                    `DAppE` (DVarE composeName
                               `DAppE` showsPrecE (con_prec + 1) argL
                               `DAppE` (DVarE composeName
                                          `DAppE` infixOpE
                                          `DAppE` showsPrecE (con_prec + 1) argR))

        DNormalC _ tys -> do
          args   <- mapM (const $ newUniqueName "arg")   tys
          argTys <- mapM (const $ newUniqueName "argTy") tys
          let show_args     = map (showsPrecE appPrec1) args
              composed_args = foldr1 (\v q -> DVarE composeName
                                               `DAppE` v
                                               `DAppE` (DVarE composeName
                                                         `DAppE` DVarE showSpaceName
                                                         `DAppE` q)) show_args
              named_args = DVarE composeName
                             `DAppE` (DVarE showStringName
                                       `DAppE` dStringE (parenInfixConName con_name' " "))
                             `DAppE` composed_args
          return $ DClause [ DVarP p
                           , DConP con_name' $
                             zipWith (mk_Show_arg_pat mode) args argTys
                           ] $
            mk_Show_rhs_sig mode argTys $
            DVarE showParenName
              `DAppE` (DVarE gtName `DAppE` DVarE p `DAppE` dIntegerE appPrec)
              `DAppE` named_args

        -- We show a record constructor with no fields the same way we'd show a
        -- normal constructor with no fields.
        DRecC [] -> go (DNormalC False [])

        DRecC tys -> do
          args   <- mapM (const $ newUniqueName "arg")   tys
          argTys <- mapM (const $ newUniqueName "argTy") tys
          let show_args =
                concatMap (\((arg_name, _, _), arg) ->
                            let arg_name'    = case mode of
                                                 ForPromotion  -> arg_name
                                                 ForShowSing{} -> singledValueName opts arg_name
                                arg_nameBase = nameBase arg_name'
                                infix_rec    = showParen (isSym arg_nameBase)
                                                         (showString arg_nameBase) ""
                            in [ DVarE showStringName `DAppE` dStringE (infix_rec ++ " = ")
                               , showsPrecE 0 arg
                               , DVarE showCommaSpaceName
                               ])
                          (zip tys args)
              brace_comma_args =   (DVarE showCharName `DAppE` dCharE mode '{')
                                 : take (length show_args - 1) show_args
              composed_args = foldr (\x y -> DVarE composeName `DAppE` x `DAppE` y)
                                    (DVarE showCharName `DAppE` dCharE mode '}')
                                    brace_comma_args
              named_args = DVarE composeName
                             `DAppE` (DVarE showStringName
                                       `DAppE` dStringE (parenInfixConName con_name' " "))
                             `DAppE` composed_args
          return $ DClause [ DVarP p
                           , DConP con_name' $
                             zipWith (mk_Show_arg_pat mode) args argTys
                           ] $
            mk_Show_rhs_sig mode argTys $
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

dCharE :: ShowMode -> Char -> DExp
dCharE mode = DLitE . to_lit
  where
    to_lit :: Char -> Lit
    to_lit c = case mode of
                 ForPromotion  -> StringL [c] -- There aren't type-level characters yet,
                                              -- so fake it with a string
                 ForShowSing{} -> CharL c

dStringE :: String -> DExp
dStringE = DLitE . StringL

dIntegerE :: Int -> DExp
dIntegerE = DLitE . IntegerL . fromIntegral

isSym :: String -> Bool
isSym ""      = False
isSym (c : _) = startsVarSym c || startsConSym c

-----
-- ShowMode
-----

-- | Is a 'Show' instance being generated to be promoted/singled, or is it
-- being generated to create a 'Show' instance for a singleton type?
data ShowMode = ForPromotion      -- ^ For promotion/singling
              | ForShowSing Name  -- ^ For a 'Show' instance.
                                  --   Bundles the 'Name' of the data type.

-- | Turn a context like @('Show' a, 'Show' b)@ into @('ShowSing' a, 'ShowSing' b)@.
-- This is necessary for 'Show' instances for singleton types.
mkShowSingContext :: ShowMode -> DCxt -> DCxt
mkShowSingContext ForPromotion  = id
mkShowSingContext ForShowSing{} = map show_to_SingShow
  where
    show_to_SingShow :: DPred -> DPred
    show_to_SingShow = modifyConNameDType $ \n ->
                         if n == showName
                            then showSingName
                            else n

mk_Show_name :: ShowMode -> Name
mk_Show_name ForPromotion  = showName
mk_Show_name ForShowSing{} = showSingName

-- If we're creating a 'Show' instance for a singleton type, decorate the type
-- appropriately (e.g., turn @Maybe a@ into @SMaybe (z :: Maybe a)@).
-- Otherwise, return the type (@Maybe a@) unchanged.
mk_Show_inst_ty :: OptionsMonad q => ShowMode -> DType -> q DType
mk_Show_inst_ty ForPromotion           ty = pure ty
mk_Show_inst_ty (ForShowSing ty_tycon) ty = do
  opts <- getOptions
  z <- qNewName "z"
  pure $ DConT (singledDataTypeName opts ty_tycon) `DAppT` (DVarT z `DSigT` ty)

-- If we're creating a 'Show' instance for a singleton type, create a pattern
-- of the form @(sx :: Sing x)@. Otherwise, simply return the pattern @sx@.
mk_Show_arg_pat :: ShowMode -> Name -> Name -> DPat
mk_Show_arg_pat ForPromotion  arg _      = DVarP arg
mk_Show_arg_pat ForShowSing{} arg arg_ty =
  DSigP (DVarP arg) (DConT singFamilyName `DAppT` DVarT arg_ty)

-- If we're creating a 'Show' instance for a singleton type, decorate the
-- expression with an explicit signature of the form
-- @e :: (ShowSing' a_1, ..., ShowSing' a_n) => ShowS@. Otherwise, return
-- the expression (@e@) unchanged.
mk_Show_rhs_sig :: ShowMode -> [Name] -> DExp -> DExp
mk_Show_rhs_sig ForPromotion  _            e = e
mk_Show_rhs_sig ForShowSing{} arg_ty_names e =
  e `DSigE` DConstrainedT (map (DAppT (DConT showSing'Name) . DVarT) arg_ty_names)
                          (DConT showSName)
