{- Data/Singletons/CustomStar.hs

(c) Richard Eisenbeg 2012
eir@cis.upenn.edu

This file implements singletonStar, which generates a datatype Rep and associated
singleton from a list of types. The promoted version of Rep is kind * and the
Haskell types themselves. This is still very experimental, so expect unusual
results!
-} 

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data.Singletons.CustomStar where

import Language.Haskell.TH
import Data.Singletons.Util
import Data.Singletons.Promote
import Data.Singletons.Singletons
import Control.Monad

-- Produce a representation and singleton for the collection of types given
singletonStar :: [Name] -> Q [Dec]
singletonStar names = do
  kinds <- mapM getKind names
  ctors <- zipWithM (mkCtor True) names kinds
  let repDecl = DataD [] repName [] ctors
                      [mkName "Eq", mkName "Show", mkName "Read"]
  fakeCtors <- zipWithM (mkCtor False) names kinds
  eqTypeInstances <- mapM mkEqTypeInstance [ (c1, c2) | c1 <- fakeCtors,
                                                        c2 <- fakeCtors ]
  singletonDecls <- singDataD True [] repName [] fakeCtors
                              [mkName "Eq", mkName "Show", mkName "Read"]
  return $ repDecl :
           eqTypeInstances ++
           singletonDecls
  where -- get the kinds of the arguments to the tycon with the given name
        getKind :: Name -> Q [Kind]
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
        mkCtor :: Bool -> Name -> [Kind] -> Q Con
        mkCtor real name args = do
          (types, vars) <- evalForPair $ mapM kindToType args
          let ctor = NormalC ((if real then reinterpret else id) name)
                             (map (\ty -> (NotStrict, ty)) types)
          if length vars > 0
            then return $ ForallC (map PlainTV vars) [] ctor
            else return ctor

        -- demote a kind back to a type, accumulating any unbound parameters
        kindToType :: Kind -> QWithAux [Name] Type
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