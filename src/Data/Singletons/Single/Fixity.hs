{-# LANGUAGE ScopedTypeVariables #-}
module Data.Singletons.Single.Fixity where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (NameSpace(..), Quasi(..))
import Data.Singletons.Util
import Data.Singletons.Names
import Language.Haskell.TH.Desugar

-- Single a fixity declaration.
singInfixDecl :: forall q. DsMonad q => Name -> Fixity -> q (Maybe DLetDec)
singInfixDecl name fixity = do
  mb_ns <- reifyNameSpace name
  case mb_ns of
    -- If we can't find the Name for some odd reason,
    -- fall back to singValName
    Nothing        -> finish $ singValName name
    Just VarName   -> finish $ singValName name
    Just DataName  -> finish $ singDataConName name
    Just TcClsName -> do
      mb_info <- dsReify name
      case mb_info of
        Just (DTyConI DClassD{} _)
          -> finish $ singTyConName name
        _ -> pure Nothing
          -- Don't produce anything for other type constructors (type synonyms,
          -- type families, data types, etc.).
          -- See [singletons and fixity declarations], wrinkle 1.
  where
    finish :: Name -> q (Maybe DLetDec)
    finish = pure . Just . DInfixD fixity

-- Try producing singled fixity declarations for Names by reifying them
-- /without/ consulting quoted declarations. If reification fails, recover and
-- return the empty list.
-- See [singletons and fixity declarations], wrinkle 2.
singReifiedInfixDecls :: forall q. DsMonad q => [Name] -> q [DDec]
singReifiedInfixDecls = mapMaybeM trySingFixityDeclaration
  where
    trySingFixityDeclaration :: Name -> q (Maybe DDec)
    trySingFixityDeclaration name =
      qRecover (return Nothing) $ do
        mFixity <- qReifyFixity name
        case mFixity of
          Nothing     -> pure Nothing
          Just fixity -> fmap (fmap DLetDec) $ singInfixDecl name fixity

{-
Note [singletons and fixity declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Promoting and singling fixity declarations is surprisingly tricky to get right.
This Note serves as a place to document the insights learned after getting this
wrong at various points.

As a general rule, when promoting something with a fixity declaration like this
one:

  infixl 5 `foo`

singletons will produce promoted and singled versions of them:

  infixl 5 `Foo`
  infixl 5 `sFoo`

singletons will also produce fixity declarations for its defunctionalization
symbols (see Note [Fixity declarations for defunctionalization symbols] in
D.S.Promote.Defun):

  infixl 5 `FooSym0`
  infixl 5 `FooSym1`
  ...

-----
-- Wrinkle 1: When not to promote/single fixity declarations
-----

Rules are meant to be broken, and the general rule above is no exception. There
are certain cases where singletons does *not* produce promoted or singled
versions of fixity declarations:

* During promotion, fixity declarations for data types, type synonyms,
  type families, data constructors, and infix functions will not receive a
  promoted counterpart. This is because the promoted versions of these names
  are the same as the originals, so generating an extra fixity declaration for
  them would run the risk of having duplicates, which GHC would reject with an
  error. (See #326 for the drawback to this approach.)

* During singling, the following things will not have their fixity declarations
  singled:

  - Type synonyms or type families. This is because singletons does not
    generate singled versions of them in the first place (they only receive
    defunctionalization symbols).

  - Data types. This is because the singled version of a data type T is
    always of the form:

      data ST :: forall a_1 ... a_n. T a_1 ... a_n -> Type where ...

    Regardless of how many arguments T has, ST will have exactly one argument.
    This makes is rather pointless to generate a fixity declaration for it.

-----
-- Wrinkle 2: Making sure fixity declarations are promoted/singled properly
-----

There are two situations where singletons must promote/single fixity
declarations:

1. When quoting code, i.e., with `promote` or `singletons`.
2. When reifying code, i.e., with `genPromotions` or `genSingletons`.

In the case of (1), singletons stores the quoted fixity declarations in the
lde_infix field of LetDecEnv. Therefore, it suffices to call
promoteInfixDecl/singleInfixDecl when processing LetDecEnvs.

In the case of (2), there is no LetDecEnv to use, so we must instead reify
the fixity declarations and promote/single those. See D.S.Single.Data.singDataD
(which singles data constructors) for a place that does this—we will use
singDataD as a running example for the rest of this section.

One complication is that code paths like singDataD are invoked in both (1) and
(2). This runs the risk that singletons will generate duplicate infix
declarations for data constructors in situation (1), as it will try to single
their fixity declarations once when processing them in LetDecEnvs and again
when reifying them in singDataD.

To avoid this pitfall, when reifying declarations in singDataD we take care
*not* to consult any quoted declarations when reifying (i.e., we do not use
reifyWithLocals for functions like it). Therefore, it we are in situation (1),
then the reification in singDataD will fail (and recover gracefully), so it
will not produce any singled fixity declarations. Therefore, the only singled
fixity declarations will be produced by processing LetDecEnvs.
-}
