{- Data/Singletons/CustomStar.hs

(c) Richard Eisenbeg 2013
eir@cis.upenn.edu

This file implements singletonStar, which generates a datatype Rep and associated
singleton from a list of types. The promoted version of Rep is kind * and the
Haskell types themselves. This is still very experimental, so expect unusual
results!
-} 
{-# LANGUAGE DataKinds, TypeFamilies, KindSignatures, CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data.Singletons.CustomStar ( singletonStar ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( Quasi(..) )
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons.Singletons
import Control.Monad

#if __GLASGOW_HASKELL__ >= 707
import Data.Singletons.Core
import Data.Singletons.Types
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

-- Produce a representation and singleton for the collection of types given
singletonStar :: Quasi q => [Name] -> q [Dec]
singletonStar names = do
  kinds <- mapM getKind names
  ctors <- zipWithM (mkCtor True) names kinds
  let repDecl = DataD [] repName [] ctors
                      [''Eq, ''Show, ''Read]
  fakeCtors <- zipWithM (mkCtor False) names kinds
  eqInstances <- mkCustomEqInstances fakeCtors
  singletonDecls <- singDataD True [] repName [] fakeCtors
                              [''Show, ''Read
#if __GLASGOW_HASKELL__ < 707
                              , ''Eq
#endif
                              ]
  return $ repDecl :
           eqInstances ++
           singletonDecls
  where -- get the kinds of the arguments to the tycon with the given name
        getKind :: Quasi q => Name -> q [Kind]
        getKind name = do
          info <- reifyWithWarning name
          case info of
            TyConI (DataD (_:_) _ _ _ _) ->
               fail "Cannot make a representation of a constrainted data type"
            TyConI (DataD [] _ tvbs _ _) ->
               return $ map extractTvbKind tvbs
            TyConI (NewtypeD (_:_) _ _ _ _) ->
               fail "Cannot make a representation of a constrainted newtype"
            TyConI (NewtypeD [] _ tvbs _ _) ->
               return $ map extractTvbKind tvbs
            TyConI (TySynD _ tvbs _) ->
               return $ map extractTvbKind tvbs
            PrimTyConI _ n _ ->
               return $ replicate n StarT
            _ -> fail $ "Invalid thing for representation: " ++ (show name)
        
        -- first parameter is whether this is a real ctor (with a fresh name)
        -- or a fake ctor (when the name is actually a Haskell type)
        mkCtor :: Quasi q => Bool -> Name -> [Kind] -> q Con
        mkCtor real name args = do
          (types, vars) <- evalForPair $ mapM kindToType args
          let ctor = NormalC ((if real then reinterpret else id) name)
                             (map (\ty -> (NotStrict, ty)) types)
          if length vars > 0
            then return $ ForallC (map PlainTV vars) [] ctor
            else return ctor

        -- demote a kind back to a type, accumulating any unbound parameters
        kindToType :: Quasi q => Kind -> QWithAux [Name] q Type
        kindToType (ForallT _ _ _) = fail "Explicit forall encountered in kind"
        kindToType (AppT k1 k2) = do
          t1 <- kindToType k1
          t2 <- kindToType k2
          return $ AppT t1 t2
        kindToType (SigT _ _) = fail "Sort signature encountered in kind"
        kindToType (VarT n) = do
          addElement n
          return $ VarT n
        kindToType (ConT n) = return $ ConT n
        kindToType (PromotedT _) = fail "Promoted type used as a kind"
        kindToType (TupleT n) = return $ TupleT n
        kindToType (UnboxedTupleT _) = fail "Unboxed tuple kind encountered"
        kindToType ArrowT = return ArrowT
        kindToType ListT = return ListT
        kindToType (PromotedTupleT _) = fail "Promoted tuple kind encountered"
        kindToType PromotedNilT = fail "Promoted nil kind encountered"
        kindToType PromotedConsT = fail "Promoted cons kind encountered"
        kindToType StarT = return $ ConT repName
        kindToType ConstraintT =
          fail $ "Cannot make a representation of a type that has " ++
                 "an argument of kind Constraint"
        kindToType (LitT _) = fail "Literal encountered at the kind level"


mkCustomEqInstances :: Quasi q => [Con] -> q [Dec]
mkCustomEqInstances ctors = do
#if __GLASGOW_HASKELL__ >= 707
  let ctorVar = error "Internal error: Equality instance inspected ctor var"
  sCtors <- evalWithoutAux $ mapM (singCtor ctorVar) ctors
  decideInst <- mkEqualityInstance StarT sCtors sDecideClassDesc

  a <- newUniqueName "a"
  b <- newUniqueName "b"
  let eqInst = InstanceD
                 []
                 (AppT (ConT ''SEq) (kindParam StarT))
                 [FunD '(%:==)
                       [Clause [VarP a, VarP b]
                               (NormalB $
                                CaseE (foldExp (VarE '(%~)) [VarE a, VarE b])
                                      [ Match (ConP 'Proved [ConP 'Refl []])
                                              (NormalB $ ConE 'STrue) []
                                      , Match (ConP 'Disproved [WildP])
                                              (NormalB $ AppE (VarE 'unsafeCoerce)
                                                              (ConE 'SFalse)) []
                                      ]) []]]
  return [decideInst, eqInst]
#else
  mapM mkEqTypeInstance [(c1, c2) | c1 <- ctors, c2 <- ctors]
#endif