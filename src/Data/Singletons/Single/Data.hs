{- Data/Singletons/Single/Data.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Singletonizes constructors.
-}

{-# LANGUAGE ParallelListComp, TupleSections, LambdaCase #-}

module Data.Singletons.Single.Data where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Type
import Data.Singletons.Promote.Type
import Data.Singletons.Single.Eq
import Data.Singletons.Util
import Data.Singletons.Names
import Data.Singletons.Syntax
import Control.Monad

-- We wish to consider the promotion of "Rep" to be *
-- not a promoted data constructor.
singDataD :: DataDecl -> SgM [DDec]
singDataD (DataDecl _nd name tvbs ctors derivings) = do
  aName <- qNewName "z"
  let a = DVarT aName
  let tvbNames = map extractTvbName tvbs
  k <- promoteType (foldType (DConT name) (map DVarT tvbNames))
  ctors' <- mapM (singCtor a) ctors

  -- instance for SingKind
  fromSingClauses <- mapM mkFromSingClause ctors
  toSingClauses   <- mapM mkToSingClause ctors
  let singKindInst =
        DInstanceD Nothing
                   (map (singKindConstraint . DVarT) tvbNames)
                   (DAppT (DConT singKindClassName)
                          (kindParam k))
                   [ DTySynInstD demoteRepName $ DTySynEqn
                      [kindParam k]
                      (foldType (DConT name)
                        (map (DAppT demote . kindParam . DVarT) tvbNames))
                   , DLetDec $ DFunD fromSingName (fromSingClauses `orIfEmpty` emptyMethod aName)
                   , DLetDec $ DFunD toSingName   (toSingClauses   `orIfEmpty` emptyMethod aName) ]

  -- SEq instance
  sEqInsts <- if any (\case DConPr n -> n == eqName; _ -> False) derivings
              then mapM (mkEqualityInstance k ctors') [sEqClassDesc, sDecideClassDesc]
              else return []

  -- e.g. type SNat = Sing :: Nat -> *
  let kindedSynInst =
        DTySynD (singTyConName name)
                []
                (singFamily `DSigT` (DArrowT `DAppT` k `DAppT` DStarT))

  return $ (DDataInstD Data [] singFamilyName [DSigT a k] ctors' []) :
           kindedSynInst :
           singKindInst :
           sEqInsts
  where -- in the Rep case, the names of the constructors are in the wrong scope
        -- (they're types, not datacons), so we have to reinterpret them.
        mkConName :: Name -> SgM Name
        mkConName
          | nameBase name == nameBase repName = mkDataName . nameBase
          | otherwise                         = return

        mkFromSingClause :: DCon -> SgM DClause
        mkFromSingClause c = do
          let (cname, numArgs) = extractNameArgs c
          cname' <- mkConName cname
          varNames <- replicateM numArgs (qNewName "b")
          return $ DClause [DConPa (singDataConName cname) (map DVarPa varNames)]
                           (foldExp
                              (DConE cname')
                              (map (DAppE (DVarE fromSingName) . DVarE) varNames))

        mkToSingClause :: DCon -> SgM DClause
        mkToSingClause (DCon _tvbs _cxt cname fields _rty) = do
          let types = tysOfConFields fields
          varNames  <- mapM (const $ qNewName "b") types
          svarNames <- mapM (const $ qNewName "c") types
          promoted  <- mapM promoteType types
          cname' <- mkConName cname
          let recursiveCalls = zipWith mkRecursiveCall varNames promoted
          return $
            DClause [DConPa cname' (map DVarPa varNames)]
                    (multiCase recursiveCalls
                               (map (DConPa someSingDataName . listify . DVarPa)
                                    svarNames)
                               (DAppE (DConE someSingDataName)
                                         (foldExp (DConE (singDataConName cname))
                                                  (map DVarE svarNames))))

        mkRecursiveCall :: Name -> DKind -> DExp
        mkRecursiveCall var_name ki =
          DSigE (DAppE (DVarE toSingName) (DVarE var_name))
                (DAppT (DConT someSingTypeName) ki)

        emptyMethod :: Name -> [DClause]
        emptyMethod n = [DClause [DVarPa n] (DCaseE (DVarE n) emptyMatches)]

-- refine a constructor. the first parameter is the type variable that
-- the singleton GADT is parameterized by
singCtor :: DType -> DCon -> SgM DCon
 -- polymorphic constructors are handled just
 -- like monomorphic ones -- the polymorphism in
 -- the kind is automatic
singCtor a (DCon _tvbs cxt name fields _rty)
  | not (null (filter (not . isEqPred) cxt))
  = fail "Singling of constrained constructors not yet supported"
  | otherwise
  = do
  let types = tysOfConFields fields
      sName = singDataConName name
      sCon = DConE sName
      pCon = DConT name
  indexNames <- mapM (const $ qNewName "n") types
  let indices = map DVarT indexNames
  kinds <- mapM promoteType types
  args <- zipWithM buildArgType types indices
  let tvbs = zipWith DKindedTV indexNames kinds
      kindedIndices = zipWith DSigT indices kinds

  -- SingI instance
  emitDecs
    [DInstanceD Nothing
                (map (DAppPr (DConPr singIName)) indices)
                (DAppT (DConT singIName)
                       (foldType pCon kindedIndices))
                [DLetDec $ DValD (DVarPa singMethName)
                       (foldExp sCon (map (const $ DVarE singMethName) types))]]

  let noBang    = Bang NoSourceUnpackedness NoSourceStrictness
      conFields = case fields of
                    DNormalC _ -> DNormalC $ map (noBang,) args
                    DRecC rec_fields ->
                      DRecC [ (singValName field_name, noBang, arg)
                            | (field_name, _, _) <- rec_fields
                            | arg <- args ]
  return $ DCon tvbs
                [mkEqPred a (foldType pCon indices)]
                sName
                conFields
                Nothing
  where buildArgType :: DType -> DType -> SgM DType
        buildArgType ty index = do
          (ty', _, _, _) <- singType index ty
          return ty'

        isEqPred :: DPred -> Bool
        isEqPred (DAppPr f _) = isEqPred f
        isEqPred (DSigPr p _) = isEqPred p
        isEqPred (DVarPr _)   = False
        isEqPred (DConPr n)   = n == equalityName
        isEqPred DWildCardPr  = False
