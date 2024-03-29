-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.Options
-- Copyright   :  (C) 2019 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines 'Options' that control finer details of how the Template
-- Haskell machinery works, as well as an @mtl@-like 'OptionsMonad' class
-- and an 'OptionsM' monad transformer.
--
----------------------------------------------------------------------------

module Data.Singletons.TH.Options
  ( -- * Options
    Options, defaultOptions
    -- ** Options record selectors
  , genQuotedDecs
  , genSingKindInsts
  , promotedDataTypeOrConName
  , promotedClassName
  , promotedValueName
  , singledDataTypeName
  , singledClassName
  , singledDataConName
  , singledValueName
  , defunctionalizedName
    -- ** Derived functions over Options
  , promotedTopLevelValueName
  , promotedLetBoundValueName
  , defunctionalizedName0

    -- * OptionsMonad
  , OptionsMonad(..), OptionsM, withOptions
  ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer (WriterT)
import Data.Singletons.TH.Names
import Data.Singletons.TH.Util
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Instances () -- To obtain a Quote instance for ReaderT
import Language.Haskell.TH.Syntax hiding (Lift(..))

-- | Options that control the finer details of how @singletons-th@'s Template
-- Haskell machinery works.
data Options = Options
  { genQuotedDecs :: Bool
    -- ^ If 'True', then quoted declarations will be generated alongside their
    --   promoted and singled counterparts. If 'False', then quoted
    --   declarations will be discarded.
  , genSingKindInsts :: Bool
    -- ^ If 'True', then 'SingKind' instances will be generated. If 'False',
    --   they will be omitted entirely. This can be useful in scenarios where
    --   TH-generated 'SingKind' instances do not typecheck (for instance,
    --   when generating singletons for GADTs).
  , promotedDataTypeOrConName :: Name -> Name
    -- ^ Given the name of the original data type or data constructor, produces
    --   the name of the promoted equivalent. Unlike the singling-related
    --   options, in which there are separate 'singledDataTypeName' and
    --   'singledDataConName' functions, we combine the handling of promoted
    --   data types and data constructors into a single option. This is because
    --   the names of promoted data types and data constructors can be
    --   difficult to distinguish in certain contexts without expensive
    --   compile-time checks.
    --
    --   Because of the @DataKinds@ extension, most data type and data
    --   constructor names can be used in promoted contexts without any
    --   changes. As a result, this option will act like the identity function
    --   99% of the time. There are some situations where it can be useful to
    --   override this option, however, as it can be used to promote primitive
    --   data types that do not have proper type-level equivalents, such as
    --   'Natural' and 'Text'. See the
    --   \"Arrows, 'Nat', 'Symbol', and literals\" section of the @singletons@
    --   @<https://github.com/goldfirere/singletons/blob/master/README.md README>@
    --   for more details.
  , promotedClassName :: Name -> Name
    -- ^ Given the name of the original, unrefined class, produces the name of
    --   the promoted equivalent of the class.
  , promotedValueName :: Name -> Maybe Uniq -> Name
    -- ^ Given the name of the original, unrefined value, produces the name of
    --   the promoted equivalent of the value. This is used for both top-level
    --   and @let@-bound names, and the difference is encoded in the
    --   @'Maybe' 'Uniq'@ argument. If promoting a top-level name, the argument
    --   is 'Nothing'. If promoting a @let@-bound name, the argument is
    --   @Just uniq@, where @uniq@ is a globally unique number that can be used
    --   to distinguish the name from other local definitions of the same name
    --   (e.g., if two functions both use @let x = ... in x@).
  , singledDataTypeName :: Name -> Name
    -- ^ Given the name of the original, unrefined data type, produces the name
    --   of the corresponding singleton type.
  , singledClassName :: Name -> Name
    -- ^ Given the name of the original, unrefined class, produces the name of
    --   the singled equivalent of the class.
  , singledDataConName :: Name -> Name
    -- ^ Given the name of the original, unrefined data constructor, produces
    --   the name of the corresponding singleton data constructor.
  , singledValueName :: Name -> Name
    -- ^ Given the name of the original, unrefined value, produces the name of
    --   the singled equivalent of the value.
  , defunctionalizedName :: Name -> Int -> Name
    -- ^ Given the original name and the number of parameters it is applied to
    --   (the 'Int' argument), produces a type-level function name that can be
    --   partially applied when given the same number of parameters.
    --
    --   Note that defunctionalization works over both term-level names
    --   (producing symbols for the promoted name) and type-level names
    --   (producing symbols directly for the name itself). As a result, this
    --   callback is used for names in both the term and type namespaces.
  }

-- | Sensible default 'Options'.
--
-- 'genQuotedDecs' defaults to 'True'.
-- That is, quoted declarations are generated alongside their promoted and
-- singled counterparts.
--
-- 'genSingKindInsts' defaults to 'True'.
-- That is, 'SingKind' instances are generated.
--
-- The default behaviors for 'promotedClassName', 'promotedValueNamePrefix',
-- 'singledDataTypeName', 'singledClassName', 'singledDataConName',
-- 'singledValueName', and 'defunctionalizedName' are described in the
-- \"On names\" section of the @singletons@
-- @<https://github.com/goldfirere/singletons/blob/master/README.md README>@.
defaultOptions :: Options
defaultOptions = Options
  { genQuotedDecs             = True
  , genSingKindInsts          = True
  , promotedDataTypeOrConName = promoteDataTypeOrConName
  , promotedClassName         = promoteClassName
  , promotedValueName         = promoteValNameLhs
  , singledDataTypeName       = singTyConName
  , singledClassName          = singClassName
  , singledDataConName        = singDataConName
  , singledValueName          = singValName
  , defunctionalizedName      = promoteTySym
  }

-- | Given the name of the original, unrefined, top-level value, produces the
-- name of the promoted equivalent of the value.
promotedTopLevelValueName :: Options -> Name -> Name
promotedTopLevelValueName opts name = promotedValueName opts name Nothing

-- | Given the name of the original, unrefined, @let@-bound value and its
-- globally unique number, produces the name of the promoted equivalent of the
-- value.
promotedLetBoundValueName :: Options -> Name -> Uniq -> Name
promotedLetBoundValueName opts name = promotedValueName opts name . Just

-- | Given the original name of a function (term- or type-level), produces a
-- type-level function name that can be partially applied even without being
-- given any arguments (i.e., @0@ arguments).
defunctionalizedName0 :: Options -> Name -> Name
defunctionalizedName0 opts name = defunctionalizedName opts name 0

-- | Class that describes monads that contain 'Options'.
class DsMonad m => OptionsMonad m where
  getOptions :: m Options

instance OptionsMonad Q where
  getOptions = pure defaultOptions

instance OptionsMonad m => OptionsMonad (DsM m) where
  getOptions = lift getOptions

instance (OptionsMonad q, Monoid m) => OptionsMonad (QWithAux m q) where
  getOptions = lift getOptions

instance OptionsMonad m => OptionsMonad (ReaderT r m) where
  getOptions = lift getOptions

instance OptionsMonad m => OptionsMonad (StateT s m) where
  getOptions = lift getOptions

instance (OptionsMonad m, Monoid w) => OptionsMonad (WriterT w m) where
  getOptions = lift getOptions

instance (OptionsMonad m, Monoid w) => OptionsMonad (RWST r w s m) where
  getOptions = lift getOptions

-- | A convenient implementation of the 'OptionsMonad' class. Use by calling
-- 'withOptions'.
newtype OptionsM m a = OptionsM (ReaderT Options m a)
  deriving ( Functor, Applicative, Monad, MonadTrans
           , Quote, Quasi, MonadFail, MonadIO, DsMonad )

-- | Turn any 'DsMonad' into an 'OptionsMonad'.
instance DsMonad m => OptionsMonad (OptionsM m) where
  getOptions = OptionsM ask

-- | Declare the 'Options' that a TH computation should use.
withOptions :: Options -> OptionsM m a -> m a
withOptions opts (OptionsM x) = runReaderT x opts

-- Used when a value name appears in a pattern context.
-- Works only for proper variables (lower-case names).
--
-- If the Maybe Uniq argument is Nothing, then the name is top-level (and
-- thus globally unique on its own).
-- If the Maybe Uniq argument is `Just uniq`, then the name is let-bound and
-- should use `uniq` to make the promoted name globally unique.
promoteValNameLhs :: Name -> Maybe Uniq -> Name
promoteValNameLhs n mb_let_uniq
    -- We can't promote promote idenitifers beginning with underscores to
    -- type names, so we work around the issue by prepending "US" at the
    -- front of the name (#229).
  | Just (us, rest) <- splitUnderscores (nameBase n)
  = mkName $ alpha ++ "US" ++ us ++ rest

  | otherwise
  = mkName $ toUpcaseStr pres n
  where
    pres = maybe noPrefix (uniquePrefixes "Let" "<<<") mb_let_uniq
    (alpha, _) = pres

-- generates type-level symbol for a given name. Int parameter represents
-- saturation: 0 - no parameters passed to the symbol, 1 - one parameter
-- passed to the symbol, and so on. Works on both promoted and unpromoted
-- names.
promoteTySym :: Name -> Int -> Name
promoteTySym name sat
      -- We can't promote promote idenitifers beginning with underscores to
      -- type names, so we work around the issue by prepending "US" at the
      -- front of the name (#229).
    | Just (us, rest) <- splitUnderscores (nameBase name)
    = default_case (mkName $ "US" ++ us ++ rest)

    | name == nilName
    = mkName $ "NilSym" ++ (show sat)

       -- Treat unboxed tuples like tuples.
       -- See Note [Promoting and singling unboxed tuples].
    | Just degree <- tupleNameDegree_maybe name <|>
                     unboxedTupleNameDegree_maybe name
    = mkName $ "Tuple" ++ show degree ++ "Sym" ++ show sat

    | otherwise
    = default_case name
  where
    default_case :: Name -> Name
    default_case name' =
      let capped = toUpcaseStr noPrefix name' in
      if isHsLetter (headNameStr capped)
      then mkName (capped ++ "Sym" ++ (show sat))
      else mkName (capped ++ "@#@" -- See Note [Defunctionalization symbol suffixes]
                          ++ (replicate (sat + 1) '$'))

promoteClassName :: Name -> Name
promoteClassName = prefixName "P" "#"

promoteDataTypeOrConName :: Name -> Name
promoteDataTypeOrConName nm
  | nameBase nm == nameBase repName = typeKindName
    -- See Note [Promoting and singling unboxed tuples]
  | Just degree <- unboxedTupleNameDegree_maybe nm
  = if isDataName nm then tupleDataName degree else tupleTypeName degree
  | otherwise = nm
  where
    -- Is this name a data constructor name? A 'False' answer means "unsure".
    isDataName :: Name -> Bool
    isDataName (Name _ (NameG DataName _ _)) = True
    isDataName _                             = False

-- Singletons

singDataConName :: Name -> Name
singDataConName nm
  | nm == nilName                                  = mkName "SNil"
  | nm == consName                                 = mkName "SCons"
  | Just degree <- tupleNameDegree_maybe nm        = mkTupleName degree
    -- See Note [Promoting and singling unboxed tuples]
  | Just degree <- unboxedTupleNameDegree_maybe nm = mkTupleName degree
  | otherwise                                      = prefixConName "S" "%" nm

singTyConName :: Name -> Name
singTyConName name
  | name == listName                                 = mkName "SList"
  | Just degree <- tupleNameDegree_maybe name        = mkTupleName degree
    -- See Note [Promoting and singling unboxed tuples]
  | Just degree <- unboxedTupleNameDegree_maybe name = mkTupleName degree
  | otherwise                                        = prefixName "S" "%" name

mkTupleName :: Int -> Name
mkTupleName n = mkName $ "STuple" ++ show n

singClassName :: Name -> Name
singClassName = singTyConName

singValName :: Name -> Name
singValName n
     -- Push the 's' past the underscores, as this lets us avoid some unused
     -- variable warnings (#229).
  | Just (us, rest) <- splitUnderscores (nameBase n)
  = prefixName (us ++ "s") "%" $ mkName rest
  | otherwise
  = prefixName "s" "%" $ upcase n

{-
Note [Promoting and singling unboxed tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unfortunately, today's GHC is not quite up to the task of promoting types
involving unboxed tuples. Consider this example:

  swapperino :: (# a, b #) -> (# b, a #)

What would this look like when promoted? Presumably, it would have a kind
signature like this:

  type Swapperino :: (# a, b #) -> (# b, a #)

Surprisingly, this won't kindcheck:

  error:
      • Expecting a lifted type, but ‘(# a, b #)’ is unlifted
      • In a standalone kind signature for ‘Swapperino’:
          (# a, b #) -> (# b, a #)

Even though (->) is levity polymorphic, this levity polymorphism only kicks in
for types, not kinds. In other words, the (->) in the kind of Swapperino is
completely levity monomorphic and only accepts Type-kinded arguments. This
oddity is tracked upstream as GHC#14180. Until that is fixed, there is no hope
of using promoted unboxed tuples freely in kinds.

However, we don't have to give up quite yet. As a crude-but-effective
workaround, we can simply promote value-level unboxed tuples to type-level boxed
tuples. In other words, we would promote swapperino to this:

  type Swapperino :: (a, b) -> (b, a)

This trick is enough to make many (but not all) uses of unboxed tuples
Just Work™ when promoted. We use a similar trick when singling unboxed tuples
as well.
-}
