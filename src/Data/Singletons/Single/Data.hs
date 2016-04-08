{- Data/Singletons/Single/Data.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Singletonizes constructors.
-}

{-# LANGUAGE ParallelListComp, TupleSections, CPP #-}

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
#if MIN_VERSION_th_desugar(1,6,0)
        DInstanceD (map (singKindConstraint . DVarT) tvbNames)
#else
        DInstanceD (map (singKindConstraint . DVarK) tvbNames)
#endif
                   (DAppT (DConT singKindClassName)
                          (kindParam k))
                   [ DTySynInstD demoteRepName $ DTySynEqn
                      [kindParam k]
                      (foldType (DConT name)
#if MIN_VERSION_th_desugar(1,6,0)
                        (map (DAppT demote . kindParam . DVarT) tvbNames))
#else
                        (map (DAppT demote . kindParam . DVarK) tvbNames))
#endif
                   , DLetDec $ DFunD fromSingName (fromSingClauses `orIfEmpty` emptyMethod aName)
                   , DLetDec $ DFunD toSingName   (toSingClauses   `orIfEmpty` emptyMethod aName) ]

  -- SEq instance
  sEqInsts <- if elem eqName derivings
              then mapM (mkEqualityInstance k ctors') [sEqClassDesc, sDecideClassDesc]
              else return []

  -- e.g. type SNat = Sing :: Nat -> *
  let kindedSynInst =
        DTySynD (singTyConName name)
                []
#if MIN_VERSION_th_desugar(1,6,0)
                (singFamily `DSigT` (foldType DArrowT [k,DStarT]))
#else
                (singFamily `DSigT` (k `DArrowK` DStarK))
#endif

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
#if MIN_VERSION_th_desugar(1,6,0)
        mkToSingClause (DCon _tvbs _cxt cname fields _m_kind) = do
#else
        mkToSingClause (DCon _tvbs _cxt cname fields) = do
#endif
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
                (DAppT (DConT someSingTypeName) (kindParam ki))

        emptyMethod :: Name -> [DClause]
        emptyMethod n = [DClause [DVarPa n] (DCaseE (DVarE n) emptyMatches)]

-- refine a constructor. the first parameter is the type variable that
-- the singleton GADT is parameterized by
singCtor :: DType -> DCon -> SgM DCon
 -- polymorphic constructors are handled just
 -- like monomorphic ones -- the polymorphism in
 -- the kind is automatic
#if MIN_VERSION_th_desugar(1,6,0)
singCtor a (DCon _tvbs cxt name fields m_kind)
#else
singCtor a (DCon _tvbs cxt name fields)
#endif
  | not (null cxt)
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
    [DInstanceD (map (DAppPr (DConPr singIName)) indices)
                (DAppT (DConT singIName)
                       (foldType pCon kindedIndices))
                [DLetDec $ DValD (DVarPa singMethName)
                       (foldExp sCon (map (const $ DVarE singMethName) types))]]

#if MIN_VERSION_th_desugar(1,6,0)
  let conFields = case fields of
                    DNormalC _ -> DNormalC $ map (Bang NoSourceUnpackedness NoSourceStrictness,) args
                    DRecC rec_fields ->
                      DRecC [ (singValName field_name, Bang NoSourceUnpackedness NoSourceStrictness, arg)
                            | (field_name, _, _) <- rec_fields
                            | arg <- args ]
  m_pKind <- traverse promoteType m_kind -- TODO: is this the right thing???
  return $ DCon tvbs
                [mkEqPred a (foldType pCon indices)]
                sName
                conFields
                m_pKind
#else
  let conFields = case fields of
                    DNormalC _ -> DNormalC $ map (NotStrict,) args
                    DRecC rec_fields ->
                      DRecC [ (singValName field_name, NotStrict, arg)
                            | (field_name, _, _) <- rec_fields
                            | arg <- args ]
  return $ DCon tvbs
                [mkEqPred a (foldType pCon indices)]
                sName
                conFields
#endif
  where buildArgType :: DType -> DType -> SgM DType
        buildArgType ty index = do
          (ty', _, _, _) <- singType index ty
          return ty'
