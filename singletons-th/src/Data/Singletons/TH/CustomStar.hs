{-# LANGUAGE TemplateHaskellQuotes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.CustomStar
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file implements 'singletonStar', which generates a datatype @Rep@ and associated
-- singleton from a list of types. The promoted version of @Rep@ is kind @*@ and the
-- Haskell types themselves. This is still very experimental, so expect unusual
-- results!
--
-- See also @Data.Singletons.Base.CustomStar@ from @singletons-base@, a
-- variant of this module that also re-exports related definitions from
-- @Prelude.Singletons@.
--
----------------------------------------------------------------------------

module Data.Singletons.TH.CustomStar (
  singletonStar,

  module Data.Singletons.TH
  ) where

import Language.Haskell.TH
import Data.Singletons.TH
import Data.Singletons.TH.Deriving.Eq
import Data.Singletons.TH.Deriving.Infer
import Data.Singletons.TH.Deriving.Ord
import Data.Singletons.TH.Deriving.Show
import Data.Singletons.TH.Promote
import Data.Singletons.TH.Promote.Monad
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Single
import Data.Singletons.TH.Single.Data
import Data.Singletons.TH.Single.Monad
import Data.Singletons.TH.Syntax
import Data.Singletons.TH.Util
import Control.Monad
import Data.Maybe
import Language.Haskell.TH.Desugar

-- | Produce a representation and singleton for the collection of types given.
--
-- A datatype @Rep@ is created, with one constructor per type in the declared
-- universe. When this type is promoted by the @singletons-th@ library, the
-- constructors become full types in @*@, not just promoted data constructors.
--
-- For example,
--
-- > $(singletonStar [''Nat, ''Bool, ''Maybe])
--
-- generates the following:
--
-- > data Rep = Nat | Bool | Maybe Rep deriving (Eq, Ord, Read, Show)
--
-- and its singleton. However, because @Rep@ is promoted to @*@, the singleton
-- is perhaps slightly unexpected:
--
-- > data SRep (a :: *) where
-- >   SNat :: Sing Nat
-- >   SBool :: Sing Bool
-- >   SMaybe :: Sing a -> Sing (Maybe a)
-- > type instance Sing = SRep
--
-- The unexpected part is that @Nat@, @Bool@, and @Maybe@ above are the real @Nat@,
-- @Bool@, and @Maybe@, not just promoted data constructors.
--
-- Please note that this function is /very/ experimental. Use at your own risk.
singletonStar :: OptionsMonad q
              => [Name]        -- ^ A list of Template Haskell @Name@s for types
              -> q [Dec]
singletonStar names = do
  kinds <- mapM getKind names
  ctors <- zipWithM (mkCtor True) names kinds
  let repDecl = DDataD Data [] repName [] (Just (DConT typeKindName)) ctors
                         [DDerivClause Nothing (map DConT [''Eq, ''Ord, ''Read, ''Show])]
  fakeCtors <- zipWithM (mkCtor False) names kinds
  let dataDecl = DataDecl Data repName [] fakeCtors
  -- Why do we need withLocalDeclarations here? It's because we end up
  -- expanding type synonyms when deriving instances for Rep, which requires
  -- reifying Rep itself. Since Rep hasn't been spliced in yet, we must put it
  -- into the local declarations.
  withLocalDeclarations [decToTH repDecl] $ do
    -- We opt to infer the constraints for the Eq instance here so that when it's
    -- promoted, Rep will be promoted to Type.
    dataDeclEqCxt <- inferConstraints (DConT ''Eq) (DConT repName) fakeCtors
    let dataDeclEqInst = DerivedDecl (Just dataDeclEqCxt) (DConT repName) repName dataDecl
    eqInst   <- mkEqInstance Nothing (DConT repName) dataDecl
    ordInst  <- mkOrdInstance Nothing (DConT repName) dataDecl
    showInst <- mkShowInstance Nothing (DConT repName) dataDecl
    (pInsts, promDecls) <- promoteM [] $ do _ <- promoteDataDec dataDecl
                                            traverse (promoteInstanceDec mempty mempty)
                                              [eqInst, ordInst, showInst]
    singletonDecls <- singDecsM [] $ do decs1 <- singDataD dataDecl
                                        decs2 <- singDerivedEqDecs dataDeclEqInst
                                        decs3 <- traverse singInstD pInsts
                                        return (decs1 ++ decs2 ++ decs3)
    return $ decsToTH $ repDecl :
                        promDecls ++
                        singletonDecls
  where -- get the kinds of the arguments to the tycon with the given name
        getKind :: DsMonad q => Name -> q [DKind]
        getKind name = do
          info <- reifyWithLocals name
          dinfo <- dsInfo info
          case dinfo of
            DTyConI (DDataD _ (_:_) _ _ _ _ _) _ ->
               fail "Cannot make a representation of a constrained data type"
            DTyConI (DDataD _ [] _ tvbs mk _ _) _ -> do
               all_tvbs <- buildDataDTvbs tvbs mk
               return $ map (fromMaybe (DConT typeKindName) . extractTvbKind) all_tvbs
            DTyConI (DTySynD _ tvbs _) _ ->
               return $ map (fromMaybe (DConT typeKindName) . extractTvbKind) tvbs
            DPrimTyConI _ n _ ->
               return $ replicate n $ DConT typeKindName
            _ -> fail $ "Invalid thing for representation: " ++ (show name)

        -- first parameter is whether this is a real ctor (with a fresh name)
        -- or a fake ctor (when the name is actually a Haskell type)
        mkCtor :: DsMonad q => Bool -> Name -> [DKind] -> q DCon
        mkCtor real name args = do
          (types, vars) <- evalForPair $ mapM (kindToType []) args
          dataName <- if real then mkDataName (nameBase name) else return name
          return $ DCon (map (`DPlainTV` SpecifiedSpec) vars) [] dataName
                        (DNormalC False (map (\ty -> (noBang, ty)) types))
                        (DConT repName)
            where
              noBang = Bang NoSourceUnpackedness NoSourceStrictness

        -- demote a kind back to a type, accumulating any unbound parameters
        kindToType :: DsMonad q => [DTypeArg] -> DKind -> QWithAux [Name] q DType
        kindToType _    (DForallT _ _)      = fail "Explicit forall encountered in kind"
        kindToType _    (DConstrainedT _ _) = fail "Explicit constraint encountered in kind"
        kindToType args (DAppT f a) = do
          a' <- kindToType [] a
          kindToType (DTANormal a' : args) f
        kindToType args (DAppKindT f a) = do
          a' <- kindToType [] a
          kindToType (DTyArg a' : args) f
        kindToType args (DSigT t k) = do
          t' <- kindToType [] t
          k' <- kindToType [] k
          return $ DSigT t' k' `applyDType` args
        kindToType args (DVarT n) = do
          addElement n
          return $ DVarT n `applyDType` args
        kindToType args (DConT n)    = return $ DConT name `applyDType` args
          where name | isTypeKindName n = repName
                     | otherwise        = n
        kindToType args DArrowT      = return $ DArrowT    `applyDType` args
        kindToType args k@(DLitT {}) = return $ k          `applyDType` args
        kindToType args DWildCardT   = return $ DWildCardT `applyDType` args
