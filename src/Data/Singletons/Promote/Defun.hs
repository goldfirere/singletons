{- Data/Singletons/Promote/Defun.hs

(c) Richard Eisenberg, Jan Stolarek 2014
eir@cis.upenn.edu

This file creates defunctionalization symbols for types during promotion.
-}

{-# LANGUAGE TemplateHaskell #-}

module Data.Singletons.Promote.Defun where

import Language.Haskell.TH.Desugar
import Data.Singletons.Promote.Monad
import Data.Singletons.Promote.Type
import Data.Singletons.Names
import Language.Haskell.TH.Syntax
import Data.Singletons.Util
import Control.Monad

defunInfo :: DInfo -> PrM [DDec]
defunInfo (DTyConI dec _instances) = buildDefunSyms dec
defunInfo (DPrimTyConI _name _numArgs _unlifted) =
  fail $ "Building defunctionalization symbols of primitive " ++
         "type constructors not supported"
defunInfo (DVarI _name _ty _mdec _fixity) =
  fail "Building defunctionalization symbols of values not supported"
defunInfo (DTyVarI _name _ty) =
  fail "Building defunctionalization symbols of type variables not supported"

buildDefunSyms :: DDec -> PrM [DDec]
buildDefunSyms (DDataD _new_or_data _cxt tyName tvbs ctors _derivings) =
  buildDefunSymsDataD tyName tvbs ctors
buildDefunSyms (DClosedTypeFamilyD name tvbs returnK_maybe _) = do
  let arg_m_kinds = map extractTvbKind tvbs
  defunctionalize name arg_m_kinds returnK_maybe
buildDefunSyms (DFamilyD TypeFam name tvbs returnK_maybe) = do
  let arg_kinds = map (default_to_star . extractTvbKind) tvbs
      res_kind  = default_to_star returnK_maybe
      default_to_star Nothing  = Just DStarK
      default_to_star (Just k) = Just k
  defunctionalize name arg_kinds res_kind
buildDefunSyms _ = fail $ "Defunctionalization symbols can only be built for " ++
                          "type families and data declarations"

buildDefunSymsDataD :: Name -> [DTyVarBndr] -> [DCon] -> PrM [DDec]
buildDefunSymsDataD tyName tvbs ctors = do
  let kis = map (DVarK . extractTvbName) tvbs
      promotedKind = DConK tyName kis
  concatMapM (promoteCtor promotedKind) ctors
  where
    promoteCtor :: DKind -> DCon -> PrM [DDec]
    promoteCtor promotedKind ctor = do
      let (name, arg_tys) = extractNameTypes ctor
      arg_kis <- mapM promoteType arg_tys
      defunctionalize name (map Just arg_kis) (Just promotedKind)

-- Generate data declarations and apply instances
-- required for defunctionalization.
-- For a type family:
--
-- type family Foo (m :: Nat) (n :: Nat) (l :: Nat) :: Nat
--
-- we generate data declarations that allow us to talk about partial
-- application at the type level:
--
-- type FooSym3 a b c = Foo a b c
-- data FooSym2 :: Nat -> Nat -> (TyFun Nat Nat) -> *
-- type instance Apply (FooSym2 a b) c = FooSym3 a b c
-- data FooSym1 :: Nat -> (TyFun Nat (TyFun Nat Nat -> *)) -> *
-- type instance Apply (FooSym1 a) b = FooSym2 a b
-- data FooSym0 :: TyFun Nat (TyFun Nat (TyFun Nat Nat -> *) -> *) -> *
-- type instance Apply FooSym0 a = FooSym1 a
--
-- defunctionalize takes list of kinds of the type family parameters and
-- kind of the result.
defunctionalize :: Name -> [Maybe DKind] -> Maybe DKind -> PrM [DDec]
defunctionalize name m_arg_kinds m_res_kind = do
  let num_args = length m_arg_kinds
      sat_name = promoteTySym name num_args
  tvbNames <- replicateM num_args $ qNewName "t"
  let sat_dec = DTySynD sat_name (zipWith mk_tvb tvbNames m_arg_kinds)
                        (foldType (DConT name) (map DVarT tvbNames))
  other_decs <- go (num_args - 1) (reverse m_arg_kinds) m_res_kind
  return $ sat_dec : other_decs
  where
    mk_tvb :: Name -> Maybe DKind -> DTyVarBndr
    mk_tvb tvb_name Nothing  = DPlainTV tvb_name
    mk_tvb tvb_name (Just k) = DKindedTV tvb_name k
    
    go :: Int -> [Maybe DKind] -> Maybe DKind -> PrM [DDec]
    go _ [] _ = return []
    go n (m_arg : m_args) m_result = do
      decls <- go (n - 1) m_args (addStar_maybe (buildTyFun_maybe m_arg m_result))
      fst_name : rest_names <- replicateM (n + 1) (qNewName "l")
      extra_name <- qNewName "arg"
      let data_name   = promoteTySym name n
          next_name   = promoteTySym name (n+1)
          con_name    = suffixName "KindInference" "###" data_name
          m_tyfun     = buildTyFun_maybe m_arg m_result
          arg_params  = zipWith mk_tvb rest_names (reverse m_args)
          tyfun_param = mk_tvb fst_name m_tyfun
          arg_names   = map extractTvbName arg_params
          params      = arg_params ++ [tyfun_param]
          con_eq_ct   = foldl DAppPr (DConPr equalityName)
                          [ foldType (DConT data_name) (map DVarT arg_names)
                            `apply`
                            (DVarT extra_name)
                          , foldType (DConT next_name) (map DVarT (arg_names ++ [extra_name]))
                          ]
          con_decl    = DCon [DPlainTV extra_name]
                             [con_eq_ct]
                             con_name
                             (DNormalC [( NotStrict
                                       , DConT proxyTypeName `DAppT`
                                         DVarT extra_name )])
          data_decl   = DDataD Data [] data_name params [con_decl] []
          app_eqn     = DTySynEqn [ foldType (DConT data_name)
                                             (map DVarT rest_names)
                                  , DVarT fst_name ]
                                  (foldType (DConT (promoteTySym name (n+1)))
                                            (map DVarT (rest_names ++ [fst_name])))
          app_decl    = DTySynInstD applyName app_eqn
          suppress    = DInstanceD [] (DConT suppressClassName `DAppT` DConT data_name)
                          [DLetDec $ DFunD suppressMethodName
                                           [DClause [DWildPa]
                                                    ((DVarE 'snd) `DAppE`
                                                     mkTupleDExp [DConE con_name,
                                                                  mkTupleDExp []])]]
      return $ suppress : data_decl : app_decl : decls

buildTyFun :: DKind -> DKind -> DKind
buildTyFun k1 k2 = DConK tyFunName [k1, k2]
    
buildTyFun_maybe :: Maybe DKind -> Maybe DKind -> Maybe DKind
buildTyFun_maybe m_k1 m_k2 = do
  k1 <- m_k1
  k2 <- m_k2
  return $ DConK tyFunName [k1, k2]

-- Counts the arity of type level function represented with TyFun constructors
-- TODO: this and isTuFun looks like a good place to use PatternSynonyms
tyFunArity :: DKind -> Int
tyFunArity (DArrowK (DConK tyFunNm [_, b]) DStarK)
  | tyFunName == tyFunNm
  = 1 + tyFunArity b
tyFunArity _ = 0

-- Checks if type is (TyFun a b -> *)
isTyFun :: DKind -> Bool
isTyFun (DArrowK (DConK tyFunNm [_,_]) DStarK)
  | tyFunName == tyFunNm
  = True
isTyFun _ = False

-- Build TyFun kind from the list of kinds
ravelTyFun :: [DKind] -> DKind
ravelTyFun []    = error "Internal error: TyFun raveling nil"
ravelTyFun [k]   = k
ravelTyFun kinds = go tailK (buildTyFun k2 k1)
    where (k1 : k2 : tailK) = reverse kinds
          go []     acc = addStar acc
          go (k:ks) acc = go ks (buildTyFun k (addStar acc))
