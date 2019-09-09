{- Data/Singletons/Single/Data.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

Singletonizes constructors.
-}

{-# LANGUAGE ParallelListComp, TupleSections, LambdaCase #-}

module Data.Singletons.Single.Data where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax
import Data.Singletons.Single.Defun
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Type
import Data.Singletons.Single.Fixity
import Data.Singletons.Promote.Type
import Data.Singletons.Util
import Data.Singletons.Names
import Data.Singletons.Syntax
import Control.Monad

-- We wish to consider the promotion of "Rep" to be *
-- not a promoted data constructor.
singDataD :: DataDecl -> SgM [DDec]
singDataD (DataDecl name tvbs ctors) = do
  let tvbNames = map extractTvbName tvbs
  k <- promoteType (foldType (DConT name) (map DVarT tvbNames))
  ctors' <- mapM (singCtor name) ctors
  ctorFixities <- singReifiedInfixDecls [ n | DCon _ _ n _ _ <- ctors ]
  -- instance for SingKind
  fromSingClauses     <- mapM mkFromSingClause ctors
  emptyFromSingClause <- mkEmptyFromSingClause
  toSingClauses       <- mapM mkToSingClause ctors
  emptyToSingClause   <- mkEmptyToSingClause
  let singKindInst =
        DInstanceD Nothing Nothing
                   (map (singKindConstraint . DVarT) tvbNames)
                   (DAppT (DConT singKindClassName) k)
                   [ DTySynInstD $ DTySynEqn Nothing
                      (DConT demoteName `DAppT` k)
                      (foldType (DConT name)
                        (map (DAppT demote . DVarT) tvbNames))
                   , DLetDec $ DFunD fromSingName
                               (fromSingClauses `orIfEmpty` [emptyFromSingClause])
                   , DLetDec $ DFunD toSingName
                               (toSingClauses   `orIfEmpty` [emptyToSingClause]) ]

  let singDataName = singTyConName name
      -- e.g. type instance Sing @Nat = SNat
      singSynInst =
        DTySynInstD $ DTySynEqn Nothing
                                (DConT singFamilyName `DAppKindT` k)
                                (DConT singDataName)
      kindedSingTy = DForallT ForallInvis (map DPlainTV tvbNames) $
                     DArrowT `DAppT` k `DAppT` DConT typeKindName

  return $ (DDataD Data [] singDataName [] (Just kindedSingTy) ctors' []) :
           singSynInst :
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
          return $ DClause [DConP (singDataConName cname) (map DVarP varNames)]
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
            DClause [DConP cname' varPats]
                    (multiCase recursiveCalls
                               (map (DConP someSingDataName . listify . DVarP)
                                    svarNames)
                               (DAppE (DConE someSingDataName)
                                         (foldExp (DConE (singDataConName cname))
                                                  (map DVarE svarNames))))

        mkToSingVarPat :: Name -> DKind -> DPat
        mkToSingVarPat varName ki =
          DSigP (DVarP varName) (DAppT (DConT demoteName) ki)

        mkRecursiveCall :: Name -> DKind -> DExp
        mkRecursiveCall var_name ki =
          DSigE (DAppE (DVarE toSingName) (DVarE var_name))
                (DAppT (DConT someSingTypeName) ki)

        mkEmptyFromSingClause :: SgM DClause
        mkEmptyFromSingClause = do
          x <- qNewName "x"
          pure $ DClause [DVarP x]
               $ DCaseE (DVarE x) []

        mkEmptyToSingClause :: SgM DClause
        mkEmptyToSingClause = do
          x <- qNewName "x"
          pure $ DClause [DVarP x]
               $ DConE someSingDataName `DAppE` DCaseE (DVarE x) []

-- Single a constructor.
singCtor :: Name -> DCon -> SgM DCon
 -- polymorphic constructors are handled just
 -- like monomorphic ones -- the polymorphism in
 -- the kind is automatic
singCtor dataName (DCon con_tvbs cxt name fields rty)
  | not (null cxt)
  = fail "Singling of constrained constructors not yet supported"
  | otherwise
  = do
  let types = tysOfConFields fields
      sName = singDataConName name
      sCon = DConE sName
      pCon = DConT name
  indexNames <- mapM (const $ qNewName "n") types
  kinds <- mapM promoteType types
  rty' <- promoteType rty
  let indices = map DVarT indexNames
      kindedIndices = zipWith DSigT indices kinds
      args = map (DAppT singFamily) indices
      kvbs = singTypeKVBs con_tvbs kinds [] rty' mempty
      all_tvbs = kvbs ++ zipWith DKindedTV indexNames kinds

  -- SingI instance for data constructor
  emitDecs
    [DInstanceD Nothing Nothing
                (map (DAppT (DConT singIName)) indices)
                (DAppT (DConT singIName)
                       (foldType pCon kindedIndices))
                [DLetDec $ DValD (DVarP singMethName)
                       (foldExp sCon (map (const $ DVarE singMethName) types))]]
  -- SingI instances for defunctionalization symbols. Note that we don't
  -- support contexts in constructors at the moment, so it's fine for now to
  -- just assume that the context is always ().
  emitDecs =<< singDefuns name DataName [] (map Just kinds) (Just rty')

  let noBang    = Bang NoSourceUnpackedness NoSourceStrictness
      conFields = case fields of
                    DNormalC dInfix _ -> DNormalC dInfix $ map (noBang,) args
                    DRecC rec_fields ->
                      DRecC [ (singValName field_name, noBang, arg)
                            | (field_name, _, _) <- rec_fields
                            | arg <- args ]
  return $ DCon all_tvbs [] sName conFields
                (DConT (singTyConName dataName) `DAppT`
                  (foldType pCon indices `DSigT` rty'))
                  -- Make sure to include an explicit `rty'` kind annotation.
                  -- See Note [Preserve the order of type variables during singling],
                  -- wrinkle 3, in D.S.Single.Type.
