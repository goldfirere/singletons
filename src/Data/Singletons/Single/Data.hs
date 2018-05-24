{- Data/Singletons/Single/Data.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

Singletonizes constructors.
-}

{-# LANGUAGE ParallelListComp, TupleSections, LambdaCase #-}

module Data.Singletons.Single.Data where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Type
import Data.Singletons.Single.Fixity
import Data.Singletons.Promote.Type
import Data.Singletons.Util
import Data.Singletons.Names
import Data.Singletons.Syntax
import Control.Monad
import qualified Data.Set as Set
import Data.Set (Set)

-- We wish to consider the promotion of "Rep" to be *
-- not a promoted data constructor.
singDataD :: DataDecl -> SgM [DDec]
singDataD (DataDecl name tvbs ctors) = do
  let tvbNames = map extractTvbName tvbs
  k <- promoteType (foldType (DConT name) (map DVarT tvbNames))
  ctors' <- mapM singCtor ctors
  ctorFixities <-
    -- try to reify the fixity declarations for the constructors and then
    -- singletonize them. In case the reification fails, we default to an
    -- empty list of singletonized fixity declarations.
    -- why this works:
    -- 1. if we're in a call to 'genSingletons', the data type was defined
    --    earlier and its constructors are in scope, the reification succeeds.
    -- 2. if we're in a call to 'singletons', the reification will fail, but
    --    the fixity declaration will get singletonized by itself (not from
    --    here, look for other invocations of 'singInfixDecl')
    singFixityDeclarations [ n | DCon _ _ n _ _ <- ctors ]
  -- instance for SingKind
  fromSingClauses     <- mapM mkFromSingClause ctors
  emptyFromSingClause <- mkEmptyFromSingClause
  toSingClauses       <- mapM mkToSingClause ctors
  emptyToSingClause   <- mkEmptyToSingClause
  let singKindInst =
        DInstanceD Nothing
                   (map (singKindConstraint . DVarT) tvbNames)
                   (DAppT (DConT singKindClassName) k)
                   [ DTySynInstD demoteName $ DTySynEqn
                      [k]
                      (foldType (DConT name)
                        (map (DAppT demote . DVarT) tvbNames))
                   , DLetDec $ DFunD fromSingName
                               (fromSingClauses `orIfEmpty` [emptyFromSingClause])
                   , DLetDec $ DFunD toSingName
                               (toSingClauses   `orIfEmpty` [emptyToSingClause]) ]

  -- e.g. type SNat = (Sing :: Nat -> Type)
  let kindedSingTy = DArrowT `DAppT` k `DAppT` DConT typeKindName
      kindedSynInst =
        DTySynD (singTyConName name)
                []
                (singFamily `DSigT` kindedSingTy)

  return $ (DDataInstD Data [] singFamilyName [] (Just kindedSingTy) ctors' []) :
           kindedSynInst :
           singKindInst :
           ctorFixities
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
          let varPats        = zipWith mkToSingVarPat varNames promoted
              recursiveCalls = zipWith mkRecursiveCall varNames promoted
          return $
            DClause [DConPa cname' varPats]
                    (multiCase recursiveCalls
                               (map (DConPa someSingDataName . listify . DVarPa)
                                    svarNames)
                               (DAppE (DConE someSingDataName)
                                         (foldExp (DConE (singDataConName cname))
                                                  (map DVarE svarNames))))

        mkToSingVarPat :: Name -> DKind -> DPat
        mkToSingVarPat varName ki =
          DSigPa (DVarPa varName) (DAppT (DConT demoteName) ki)

        mkRecursiveCall :: Name -> DKind -> DExp
        mkRecursiveCall var_name ki =
          DSigE (DAppE (DVarE toSingName) (DVarE var_name))
                (DAppT (DConT someSingTypeName) ki)

        mkEmptyFromSingClause :: SgM DClause
        mkEmptyFromSingClause = do
          x <- qNewName "x"
          pure $ DClause [DVarPa x]
               $ DCaseE (DVarE x) []

        mkEmptyToSingClause :: SgM DClause
        mkEmptyToSingClause = do
          x <- qNewName "x"
          pure $ DClause [DVarPa x]
               $ DConE someSingDataName `DAppE` DCaseE (DVarE x) []

-- refine a constructor.
singCtor :: DCon -> SgM DCon
 -- polymorphic constructors are handled just
 -- like monomorphic ones -- the polymorphism in
 -- the kind is automatic
singCtor (DCon _tvbs cxt name fields _rty)
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
  let bound_kvs = foldMap fvDType kinds
  args <- zipWithM (buildArgType bound_kvs) types indices
  let tvbs = map DPlainTV (Set.toList bound_kvs) ++ zipWith DKindedTV indexNames kinds
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
                    DNormalC dInfix _ -> DNormalC dInfix $ map (noBang,) args
                    DRecC rec_fields ->
                      DRecC [ (singValName field_name, noBang, arg)
                            | (field_name, _, _) <- rec_fields
                            | arg <- args ]
  return $ DCon tvbs
                []
                sName
                conFields
                (DConT singFamilyName `DAppT` foldType pCon indices)
  where buildArgType :: Set Name -> DType -> DType -> SgM DType
        buildArgType bound_kvs ty index = do
          (ty', _, _, _) <- singType bound_kvs index ty
          return ty'

        isEqPred :: DPred -> Bool
        isEqPred (DAppPr f _) = isEqPred f
        isEqPred (DSigPr p _) = isEqPred p
        isEqPred (DVarPr _)   = False
        isEqPred (DConPr n)   = n == equalityName
        isEqPred DWildCardPr  = False
