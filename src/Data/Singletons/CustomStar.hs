{-# LANGUAGE DataKinds, TypeFamilies, KindSignatures, CPP, TemplateHaskell #-}

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

module Data.Singletons.CustomStar (
  singletonStar,

  module Data.Singletons.Prelude.Eq,
  module Data.Singletons.Prelude.Bool
  ) where

import Language.Haskell.TH
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons.Promote.Monad
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Data
import Data.Singletons.Syntax
import Data.Singletons.Names
import Control.Monad
import Data.Maybe
import Control.Applicative
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Sweeten
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
                       [''Eq, ''Show, ''Read]
  fakeCtors <- zipWithM (mkCtor False) names kinds
  let dataDecl = DataDecl Data repName [] fakeCtors [''Show, ''Read , ''Eq, ''Ord]
  promDecls      <- promoteM_ $ promoteDataDec dataDecl
  singletonDecls <- singDecsM $ singDataD dataDecl
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
            DTyConI (DDataD _ [] _ tvbs _ _) _ ->
               return $ map (fromMaybe DStarK . extractTvbKind) tvbs
            DTyConI (DTySynD _ tvbs _) _ ->
               return $ map (fromMaybe DStarK . extractTvbKind) tvbs
            DPrimTyConI _ n _ ->
               return $ replicate n DStarK
            _ -> fail $ "Invalid thing for representation: " ++ (show name)

        -- first parameter is whether this is a real ctor (with a fresh name)
        -- or a fake ctor (when the name is actually a Haskell type)
        mkCtor :: DsMonad q => Bool -> Name -> [DKind] -> q DCon
        mkCtor real name args = do
          (types, vars) <- evalForPair $ mapM kindToType args
          dataName <- if real then mkDataName (nameBase name) else return name
          return $ DCon (map DPlainTV vars) [] dataName $
                   DNormalC (map (\ty -> (NotStrict, ty)) types)

        -- demote a kind back to a type, accumulating any unbound parameters
        kindToType :: DsMonad q => DKind -> QWithAux [Name] q DType
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

