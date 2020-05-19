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
import Data.Singletons.TH.Options
import Control.Monad

-- We wish to consider the promotion of "Rep" to be *
-- not a promoted data constructor.
singDataD :: DataDecl -> SgM [DDec]
singDataD (DataDecl name tvbs ctors) = do
  opts <- getOptions
  let tvbNames      = map extractTvbName tvbs
      ctor_names    = map extractName ctors
      rec_sel_names = concatMap extractRecSelNames ctors
  k <- promoteType (foldType (DConT name) (map DVarT tvbNames))
  mb_data_sak <- dsReifyType name
  ctors' <- mapM (singCtor name) ctors
  fixityDecs <- singReifiedInfixDecls $ ctor_names ++ rec_sel_names
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

  let singDataName = singledDataTypeName opts name
      -- e.g. type instance Sing @Nat = SNat
      singSynInst =
        DTySynInstD $ DTySynEqn Nothing
                                (DConT singFamilyName `DAppKindT` k)
                                (DConT singDataName)

      mk_data_dec tvbs' mb_kind =
        DDataD Data [] singDataName tvbs' mb_kind ctors' []

  data_decs <-
    case mb_data_sak of
      -- No standalone kind signature. Try to figure out the order of kind
      -- variables on a best-effort basis.
      Nothing ->
        let sing_tvbs = toposortTyVarsOf $ map dTyVarBndrToDType tvbs
            kinded_sing_ty = DForallT ForallInvis sing_tvbs $
                             DArrowT `DAppT` k `DAppT` DConT typeKindName in
        pure [mk_data_dec [] (Just kinded_sing_ty)]

      -- A standalone kind signature is provided, so use that to determine the
      -- order of kind variables.
      Just data_sak -> do
        let (args, _)  = unravelDType data_sak
            vis_args   = filterDVisFunArgs args
            vis_tvbs   = zipWith replaceTvbKind vis_args tvbs
            invis_args = filterInvisTvbArgs args
            -- If the standalone kind signature did not explicitly quantify its
            -- kind variables, do so ourselves. This is very similar to what
            -- D.S.Single.Type.singTypeKVBs does.
            invis_tvbs | null invis_args
                       = toposortTyVarsOf [data_sak]
                       | otherwise
                       = invis_args
        z  <- qNewName "z"
        let sing_data_sak = DForallT ForallInvis (invis_tvbs ++ vis_tvbs) $
                            DArrowT `DAppT` k `DAppT` DConT typeKindName
        pure [ DKiSigD singDataName sing_data_sak
             , mk_data_dec [DPlainTV z] Nothing
             ]

  return $ data_decs ++
           singSynInst :
           [singKindInst | genSingKindInsts opts] ++
           fixityDecs
  where -- in the Rep case, the names of the constructors are in the wrong scope
        -- (they're types, not datacons), so we have to reinterpret them.
        mkConName :: Name -> SgM Name
        mkConName
          | nameBase name == nameBase repName = mkDataName . nameBase
          | otherwise                         = return

        mkFromSingClause :: DCon -> SgM DClause
        mkFromSingClause c = do
          opts <- getOptions
          let (cname, numArgs) = extractNameArgs c
          cname' <- mkConName cname
          varNames <- replicateM numArgs (qNewName "b")
          return $ DClause [DConP (singledDataConName opts cname) (map DVarP varNames)]
                           (foldExp
                              (DConE cname')
                              (map (DAppE (DVarE fromSingName) . DVarE) varNames))

        mkToSingClause :: DCon -> SgM DClause
        mkToSingClause (DCon _tvbs _cxt cname fields _rty) = do
          opts <- getOptions
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
                                         (foldExp (DConE (singledDataConName opts cname))
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
  opts <- getOptions
  let types = tysOfConFields fields
      sName = singledDataConName opts name
      sCon = DConE sName
      pCon = DConT $ promotedDataTypeOrConName opts name
  checkVanillaDType $ ravelVanillaDType con_tvbs [] types rty
  indexNames <- mapM (const $ qNewName "n") types
  kinds <- mapM promoteType_NC types
  rty' <- promoteType_NC rty
  let indices = map DVarT indexNames
      kindedIndices = zipWith DSigT indices kinds
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
      args      = map ((noBang,) . DAppT singFamily) indices
      conFields = case fields of
                    DNormalC dInfix _ -> DNormalC dInfix args
                    DRecC _           -> DNormalC False  args
                      -- Don't bother looking at record selectors, as they are
                      -- handled separately in singTopLevelDecs.
                      -- See Note [singletons and record selectors]
  return $ DCon all_tvbs [] sName conFields
                (DConT (singledDataTypeName opts dataName) `DAppT`
                  (foldType pCon indices `DSigT` rty'))
                  -- Make sure to include an explicit `rty'` kind annotation.
                  -- See Note [Preserve the order of type variables during singling],
                  -- wrinkle 3, in D.S.Single.Type.

{-
Note [singletons and record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Record selectors are annoying to deal with in singletons for various reasons:

1. There is no record syntax at the type level, so promoting code that involves
   records in some way is not straightforward.
2. One can define record selectors for singled data types, but they're rife
   with peril. Some pitfalls include:

   * Singling record updates often produces code that does not typecheck. For
     example, this works:

       let i = Identity True in i { runIdentity = False }

     But this does /not/ work:

       let si = SIdentity STrue in si { sRunIdentity = SFalse }

       error:
           • Record update for insufficiently polymorphic field:
               sRunIdentity :: Sing n
           • In the expression: si {sRunIdentity = SFalse}
             In the expression:
               let si = SIdentity STrue in si {sRunIdentity = SFalse}

     Ugh. See GHC#16501.

   * Singling a data type with multiple constructors that share a record
     selector name will /also/ not typecheck. While this works:

       data X = X1 {y :: Bool} | X2 {y :: Bool}

     This does not:

       data SX :: X -> Type where
         SX1 :: { sY :: Sing n } -> SX ('X1 n)
         SY1 :: { sY :: Sing n } -> SX ('X2 n)

       error:
           • Constructors SX1 and SX2 have a common field ‘sY’,
               but have different result types
           • In the data type declaration for ‘SX’

     Double ugh. See GHC#8673/GHC#12159.

   * Even if a data type only has a single constructor with record selectors,
     singling it can induce headaches. One might be tempted to single this type:

       newtype Unit = MkUnit { runUnit :: () }

     With this code:

       data SUnit :: Unit -> Type where
         SMkUnit :: { sRunUnit :: Sing u } -> SUnit (MkUnit u)

     Somewhat surprisingly, the type of sRunUnit:

       sRunUnit :: Sing (MkUnit u) -> Sing u

     Is not general enough to handle common uses of record selectors. For
     example, if you try to single this function:

       f :: Unit -> ()
       f = runUnit

     Then the resulting code:

       sF :: Sing (x :: Unit) -> Sing (F x :: ())
       sF = sRunUnit

     Will not typecheck. Note that sRunUnit expects an argument of type
     `Sing (MkUnit u)`, but there is no way to know a priori that the `x` in
     `Sing (x :: Unit)` is `MkUnit u` without pattern-matching on SMkUnit.

Hopefully I have convinced you that handling records in singletons is a bit of
a nightmare. Thankfully, there is a simple trick to avoid most of the pitfalls
above: just desugar code (using th-desugar) to avoid records!
In more concrete terms, we do the following:

* A record constructions desugars to a normal constructor application. For example:

    MkT{a = x, b = y}

      ==>

    MkT x y

  Something similar occurs for record syntax in patterns.

* A record update desugars to a case expression. For example:

    t{a = x}

      ==>

    case t of MkT _ y => MkT x y

We can't easily desugar away all uses of records, however. After all, records
can be used as ordinary functions as well. We leave such uses of records alone
when desugaring and accommodate them during promotion and singling by generating
"manual" record selectors. As a running example, consider the earlier Unit example:

  newtype Unit = MkUnit { runUnit :: () }

When singling Unit, we do not give SMkUnit a record selector:

  data SUnit :: Unit -> Type where
    SMkUnit :: Sing u -> SUnit (MkUnit u)

Instead, we generate a top-level function that behaves equivalently to runUnit.
This function then gets promoted and singled (in D.S.Promote.promoteDecs and
D.S.Single.singTopLevelDecs):

  type family RunUnit (x :: Unit) :: () where
    RunUnit (MkUnit x) = x

  sRunUnit :: Sing (x :: Unit) -> Sing (RunUnit x :: ())
  sRunUnit (SMkUnit sx) = sx

Now promoting/singling uses of runUnit as an ordinary function work as expected
since the types of RunUnit/sRunUnit are sufficiently general. This technique also
scales up to data types with multiple constructors sharing a record selector name.
For instance, in the earlier X example:

  data X = X1 {y :: Bool} | X2 {y :: Bool}

We would promote/single `y` like so:

  type family Y (x :: X) :: Bool where
    Y (X1 y) = y
    Y (X2 y) = y

  sY :: Sing (x :: X) -> Sing (Y x :: Bool)
  sY (SX1 sy) = sy
  sY (SX2 sy) = sy

Manual record selectors cannot be used in record constructions or updates, but
for most use cases this won't be an issue, since singletons makes an effort to
desugar away fancy uses of records anyway. The only time this would bite is if
you wanted to use record syntax in hand-written singletons code.
-}
