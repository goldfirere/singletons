{-# LANGUAGE DataKinds, TypeFamilies, KindSignatures, TemplateHaskell, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.CustomStar
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file implements 'singletonStar', which generates a datatype @Rep@ and associated
-- singleton from a list of types. The promoted version of @Rep@ is kind @*@ and the
-- Haskell types themselves. This is still very experimental, so expect unusual
-- results!
--
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Data.Singletons.CustomStar (
  singletonStar,

  module Data.Singletons.Prelude.Eq,
  module Data.Singletons.Prelude.Bool
  ) where

import Language.Haskell.TH
import Data.Singletons.Util
import Data.Singletons.Deriving.Ord
import Data.Singletons.Promote
import Data.Singletons.Promote.Monad
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Data
import Data.Singletons.Single
import Data.Singletons.Syntax
import Data.Singletons.Names
import Control.Monad
import Data.Maybe
import Language.Haskell.TH.Desugar
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Bool

-- | Produce a representation and singleton for the collection of types given.
--
-- A datatype @Rep@ is created, with one constructor per type in the declared
-- universe. When this type is promoted by the singletons library, the
-- constructors become full types in @*@, not just promoted data constructors.
--
-- For example,
--
-- > $(singletonStar [''Nat, ''Bool, ''Maybe])
--
-- generates the following:
--
-- > data Rep = Nat | Bool | Maybe Rep deriving (Eq, Show, Read)
--
-- and its singleton. However, because @Rep@ is promoted to @*@, the singleton
-- is perhaps slightly unexpected:
--
-- > data instance Sing (a :: *) where
-- >   SNat :: Sing Nat
-- >   SBool :: Sing Bool
-- >   SMaybe :: SingRep a => Sing a -> Sing (Maybe a)
--
-- The unexpected part is that @Nat@, @Bool@, and @Maybe@ above are the real @Nat@,
-- @Bool@, and @Maybe@, not just promoted data constructors.
--
-- Please note that this function is /very/ experimental. Use at your own risk.
singletonStar :: DsMonad q
              => [Name]        -- ^ A list of Template Haskell @Name@s for types
              -> q [Dec]
singletonStar names = do
  kinds <- mapM getKind names
  ctors <- zipWithM (mkCtor True) names kinds
  let repDecl = DDataD Data [] repName [] ctors
#if MIN_VERSION_th_desugar(1,6,0)
                       (map DConPr [''Eq, ''Show, ''Read])
#else
                       [''Eq, ''Show, ''Read]
#endif
  fakeCtors <- zipWithM (mkCtor False) names kinds
  let dataDecl = DataDecl Data repName [] fakeCtors [''Show, ''Read , ''Eq]
  ordInst <- mkOrdInstance (DConT repName) fakeCtors
  (pOrdInst, promDecls) <- promoteM [] $ do promoteDataDec dataDecl
                                            promoteInstanceDec mempty ordInst
  singletonDecls <- singDecsM [] $ do decs1 <- singDataD dataDecl
                                      dec2  <- singInstD pOrdInst
                                      return (dec2 : decs1)
  return $ decsToTH $ repDecl :
                      promDecls ++
                      singletonDecls
  where -- get the kinds of the arguments to the tycon with the given name
        getKind :: DsMonad q => Name -> q [DKind]
        getKind name = do
          info <- reifyWithWarning name
          dinfo <- dsInfo info
          case dinfo of
            DTyConI (DDataD _ (_:_) _ _ _ _) _ ->
               fail "Cannot make a representation of a constrainted data type"
#if MIN_VERSION_th_desugar(1,6,0)
            DTyConI (DDataD _ [] _ tvbs _ _) _ ->
               return $ map (fromMaybe DStarT . extractTvbKind) tvbs
            DTyConI (DTySynD _ tvbs _) _ ->
               return $ map (fromMaybe DStarT . extractTvbKind) tvbs
            DPrimTyConI _ n _ ->
               return $ replicate n DStarT
#else
            DTyConI (DDataD _ [] _ tvbs _ _) _ ->
               return $ map (fromMaybe DStarK . extractTvbKind) tvbs
            DTyConI (DTySynD _ tvbs _) _ ->
               return $ map (fromMaybe DStarK . extractTvbKind) tvbs
            DPrimTyConI _ n _ ->
               return $ replicate n DStarK
#endif
            _ -> fail $ "Invalid thing for representation: " ++ (show name)

        -- first parameter is whether this is a real ctor (with a fresh name)
        -- or a fake ctor (when the name is actually a Haskell type)
        mkCtor :: DsMonad q => Bool -> Name -> [DKind] -> q DCon
        mkCtor real name args = do
          (types, vars) <- evalForPair $ mapM kindToType args
          dataName <- if real then mkDataName (nameBase name) else return name
          return $ DCon (map DPlainTV vars) [] dataName
#if MIN_VERSION_th_desugar(1,6,0)
                   (DNormalC (map (\ty -> (Bang NoSourceUnpackedness NoSourceStrictness, ty)) types))
#else
                   (DNormalC (map (\ty -> (NotStrict, ty)) types))
#endif
                   Nothing

        -- demote a kind back to a type, accumulating any unbound parameters
        kindToType :: DsMonad q => DKind -> QWithAux [Name] q DType
#if MIN_VERSION_th_desugar(1,6,0)
        kindToType (DForallT _ _ _) = fail "Explicit forall encountered in kind"
        kindToType (DVarT n) = do
          addElement n
          return $ DVarT n
        kindToType k@(DConT _) = return k
        kindToType k@(DArrowT)  = return k
        kindToType (DAppT k1 k2) = do
          t1 <- kindToType k1
          t2 <- kindToType k2
          return $ DAppT t1 t2
        kindToType (DSigT t k) = do
          t' <- kindToType t
          k' <- kindToType k
          return $ DSigT t' k'
        kindToType (DLitT _) = fail "Type literal encountered in kind"
        kindToType DWildCardT = fail "Wildcard encountered in kind"
        kindToType DStarT = return $ DConT repName
#else
        kindToType (DForallK _ _) = fail "Explicit forall encountered in kind"
        kindToType (DVarK n) = do
          addElement n
          return $ DVarT n
        kindToType (DConK n args) = foldType (DConT n) <$> mapM kindToType args
        kindToType (DArrowK k1 k2) = do
          t1 <- kindToType k1
          t2 <- kindToType k2
          return $ DAppT (DAppT DArrowT t1) t2
        kindToType DStarK = return $ DConT repName
#endif
