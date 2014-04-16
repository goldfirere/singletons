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

module Data.Singletons.CustomStar ( singletonStar ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( Quasi(..) )
import Data.Singletons.Util
import Data.Singletons.Single
import Data.Singletons.Single.Monad
import Data.Singletons.Names
import Control.Monad
import Data.Maybe
import Control.Applicative
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Sweeten

#if __GLASGOW_HASKELL__ >= 707
import Data.Singletons.Decide
import Data.Singletons.Instances
import Data.Singletons.Eq
import Unsafe.Coerce
#endif

{-
The SEq instance here is tricky.
The problem is that, in GHC 7.8+, the instance of type-level (==) for *
is not recursive. Thus, it's impossible, say, to get (Maybe a == Maybe b) ~ False
from (a == b) ~ False.

There are a few ways forward:
  1) Define SEq to use our own Boolean (==) operator, instead of the built-in one.
     This would work, but feels wrong.
  2) Use unsafeCoerce.
We do #2.

Also to note: because these problems don't exist in GHC 7.6, the generation of
Eq and Decide for 7.6 is entirely normal.

Note that mkCustomEqInstances makes the SDecide and SEq instances in GHC 7.8+,
but the type-level (==) instance in GHC 7.6. This is perhaps poor design, but
it reduces the amount of CPP noise.
-}

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
singletonStar :: Quasi q
              => [Name]        -- ^ A list of Template Haskell @Name@s for types
              -> q [Dec]
singletonStar names = do
  kinds <- mapM getKind names
  ctors <- zipWithM (mkCtor True) names kinds
  let repDecl = DDataD Data [] repName [] ctors
                       [''Eq, ''Show, ''Read]
  fakeCtors <- zipWithM (mkCtor False) names kinds
  eqInstances <- mkCustomEqInstances fakeCtors
  singletonDecls <- singDecsM $ singDataD [] repName [] fakeCtors
                              [''Show, ''Read
#if __GLASGOW_HASKELL__ < 707
                              , ''Eq
#endif
                              ]
  return $ decsToTH $ repDecl :
                      eqInstances ++
                      singletonDecls
  where -- get the kinds of the arguments to the tycon with the given name
        getKind :: Quasi q => Name -> q [DKind]
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
        mkCtor :: Quasi q => Bool -> Name -> [DKind] -> q DCon
        mkCtor real name args = do
          (types, vars) <- evalForPair $ mapM kindToType args
          dataName <- if real then mkDataName (nameBase name) else return name
          return $ DCon (map DPlainTV vars) [] dataName $
                   DNormalC (map (\ty -> (NotStrict, ty)) types)

        -- demote a kind back to a type, accumulating any unbound parameters
        kindToType :: Quasi q => DKind -> QWithAux [Name] q DType
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

mkCustomEqInstances :: Quasi q => [DCon] -> q [DDec]
mkCustomEqInstances ctors = do
#if __GLASGOW_HASKELL__ >= 707
  let ctorVar = error "Internal error: Equality instance inspected ctor var"
  (sCtors, _) <- singM $ mapM (singCtor ctorVar) ctors
  decideInst <- mkEqualityInstance DStarK sCtors sDecideClassDesc

  a <- qNewName "a"
  b <- qNewName "b"
  let eqInst = DInstanceD
                 []
                 (DAppT (DConT ''SEq) (kindParam DStarK))
                 [DLetDec $ DFunD '(%:==)
                       [DClause [DVarPa a, DVarPa b]
                               (DCaseE (foldExp (DVarE '(%~)) [DVarE a, DVarE b])
                                      [ DMatch (DConPa 'Proved [DConPa 'Refl []])
                                              (DConE 'STrue)
                                      , DMatch (DConPa 'Disproved [DWildPa])
                                              (DAppE (DVarE 'unsafeCoerce)
                                                     (DConE 'SFalse))
                                      ])]]
  return [decideInst, eqInst]
#else
  mapM mkEqTypeInstance [(c1, c2) | c1 <- ctors, c2 <- ctors]
#endif
