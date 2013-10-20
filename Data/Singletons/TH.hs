{- Data/Singletons/TH.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Exported module that contains everything you need to derive your own singletons
via Template Haskell.
-}

{-# LANGUAGE ExplicitNamespaces, CPP #-}

module Data.Singletons.TH (
  Sing, SingI(..), SingKind(..), KindOf, Demote,
  type (==),
  (:&&), Any, If, 
  SEq(..), sIf,
  SDecide(..), (:~:)(..), Void, Refuted, Decision(..),
  KProxy(..), SomeSing(..),
  singletons, singletonsOnly, genSingletons,
  promote, promoteOnly, genPromotions,
  promoteEqInstances, promoteEqInstance, singEqInstances, singEqInstance,
  singDecideInstances, singDecideInstance,
  cases ) where

import Data.Singletons.Singletons
import Data.Singletons.Promote
import Data.Singletons.Core
import Data.Singletons.Bool
import Data.Singletons.Eq
import Data.Singletons.Types

import GHC.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( Quasi(..) )
import Language.Haskell.TH.Desugar
import Data.Singletons.Util
import Control.Applicative

-- allows for automatic checking of all constructors in a GADT for instance
-- inference
cases :: Quasi q => Name -> q Exp -> q Exp -> q Exp
cases tyName expq bodyq = do
  info <- reifyWithWarning tyName
  case info of
    TyConI (DataD _ _ _ ctors _) -> buildCases ctors
    TyConI (NewtypeD _ _ _ ctor _) -> buildCases [ctor]
    _ -> fail $ "Using <<cases>> with something other than a type constructor: "
                ++ (show tyName)
  where buildCases ctors =
          CaseE <$> expq <*>
                    mapM (\con -> Match (conToPat con) <$>
                                        (NormalB <$> bodyq) <*> pure []) ctors

        conToPat :: Con -> Pat
        conToPat = ctor1Case
          (\name tys -> ConP name (map (const WildP) tys))
