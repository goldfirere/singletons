{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TypeError
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines a drop-in replacement for 'TL.TypeError' (from "GHC.TypeLits")
-- that can be used at the value level as well. Since this is a drop-in
-- replacement, it is not recommended to import all of "GHC.TypeLits"
-- and "Data.Singletons.TypeError" at the same time, as many of the definitons
-- in the latter deliberately clash with the former.
--
----------------------------------------------------------------------------
module Data.Singletons.TypeError (
  TypeError, sTypeError, typeError,
  ErrorMessage'(..), ErrorMessage, PErrorMessage,
  Sing(SText, SShowType, (:%<>:), (:%$$:)), SErrorMessage,
  ConvertPErrorMessage, showErrorMessage,

  -- * Defunctionalization symbols
  TextSym0, TextSym1,
  ShowTypeSym0, ShowTypeSym1,
  type (:<>:@#@$), type (:<>:@#@$$), type (:<>:@#@$$$),
  type (:$$:@#@$), type (:$$:@#@$$), type (:$$:@#@$$$),
  TypeErrorSym0, TypeErrorSym1
  ) where

import Data.Kind
import Data.Singletons.TH
import qualified Data.Text as Text
import qualified GHC.TypeLits as TL (ErrorMessage(..), TypeError)
import GHC.Stack (HasCallStack)
import GHC.TypeLits hiding (ErrorMessage(..), TypeError)
import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, (<>), ($$))

-- | A description of a custom type error.
--
-- This is a variation on 'TL.ErrorMessage' that is parameterized over what
-- text type is used in the 'Text' constructor. Instantiating it with
-- 'Text.Text' gives you 'ErrorMessage', and instantiating it with 'Symbol'
-- gives you 'PErrorMessage'.
data ErrorMessage' s
  = Text s
    -- ^ Show the text as is.
  | forall t. ShowType t
    -- ^ Pretty print the type.
    -- @ShowType :: k -> ErrorMessage@
  | ErrorMessage' s :<>: ErrorMessage' s
    -- ^ Put two pieces of error message next
    -- to each other.
  | ErrorMessage' s :$$: ErrorMessage' s
    -- ^ Stack two pieces of error message on top
    -- of each other.
infixl 6 :<>:
infixl 5 :$$:

-- | A value-level `ErrorMessage'` which uses 'Text.Text' as its text type.
type ErrorMessage  = ErrorMessage' Text.Text

-- | A type-level `ErrorMessage'` which uses 'Symbol' as its text kind.
type PErrorMessage = ErrorMessage' Symbol

data instance Sing :: PErrorMessage -> Type where
  -- It would be lovely to not have to write those (:: PErrorMessage) kind
  -- ascriptions in the return types of each constructor.
  -- See Trac #14111.
  SText     :: Sing t             -> Sing ('Text t      :: PErrorMessage)
  SShowType :: Sing ty            -> Sing ('ShowType ty :: PErrorMessage)
  (:%<>:)   :: Sing e1 -> Sing e2 -> Sing (e1 ':<>: e2  :: PErrorMessage)
  (:%$$:)   :: Sing e1 -> Sing e2 -> Sing (e1 ':$$: e2  :: PErrorMessage)
infixl 6 :%<>:
infixl 5 :%$$:

type SErrorMessage = (Sing :: PErrorMessage -> Type)

instance SingKind PErrorMessage where
  type Demote PErrorMessage = ErrorMessage
  fromSing (SText t)      = Text (fromSing t)
  fromSing (SShowType{})  = ShowType (error "Can't single ShowType")
  fromSing (e1 :%<>: e2)  = fromSing e1 :<>: fromSing e2
  fromSing (e1 :%$$: e2)  = fromSing e1 :$$: fromSing e2
  toSing (Text t)     = withSomeSing t  $ SomeSing . SText
  toSing (ShowType{}) = SomeSing $ SShowType (error "Can't single ShowType")
  toSing (e1 :<>: e2) = withSomeSing e1 $ \sE1 ->
                        withSomeSing e2 $ \sE2 ->
                        SomeSing (sE1 :%<>: sE2)
  toSing (e1 :$$: e2) = withSomeSing e1 $ \sE1 ->
                        withSomeSing e2 $ \sE2 ->
                        SomeSing (sE1 :%$$: sE2)

instance SingI t => SingI ('Text t :: PErrorMessage) where
  sing = SText sing

instance SingI ty => SingI ('ShowType ty :: PErrorMessage) where
  sing = SShowType sing

instance (SingI e1, SingI e2) => SingI (e1 ':<>: e2 :: PErrorMessage) where
  sing = sing :%<>: sing

instance (SingI e1, SingI e2) => SingI (e1 ':$$: e2 :: PErrorMessage) where
  sing = sing :%$$: sing

-- | Convert an 'ErrorMessage' into a human-readable 'String'.
showErrorMessage :: ErrorMessage -> String
showErrorMessage = show . go
  where
  go :: ErrorMessage -> Doc
  go (Text t)     = text (Text.unpack t)
  go (ShowType _) = text "<type>" -- Not much we can do here
  go (e1 :<>: e2) = go e1 <> go e2
  go (e1 :$$: e2) = go e1 $$ go e2

-- | The value-level counterpart to 'TypeError'.
--
-- Note that this is not quite as expressive as 'TypeError', as it is unable
-- to print the contents of 'ShowType' constructors (it will simply print
-- @\"\<type\>\"@ in their place).
typeError :: HasCallStack => ErrorMessage -> a
typeError = error . showErrorMessage

-- | Convert a 'PErrorMessage' to a 'TL.ErrorMessage' from "GHC.TypeLits".
type family ConvertPErrorMessage (a :: PErrorMessage) :: TL.ErrorMessage where
  ConvertPErrorMessage ('Text t)      = 'TL.Text t
  ConvertPErrorMessage ('ShowType ty) = 'TL.ShowType ty
  ConvertPErrorMessage (e1 ':<>: e2)  = ConvertPErrorMessage e1 'TL.:<>: ConvertPErrorMessage e2
  ConvertPErrorMessage (e1 ':$$: e2)  = ConvertPErrorMessage e1 'TL.:$$: ConvertPErrorMessage e2

-- | A drop-in replacement for 'TL.TypeError'. This also exists at the
-- value-level as 'typeError'.
type family TypeError (a :: PErrorMessage) :: b where
  -- We cannot define this as a type synonym due to Trac #12048.
  TypeError a = TL.TypeError (ConvertPErrorMessage a)

-- | The singleton for 'typeError'.
--
-- Note that this is not quite as expressive as 'TypeError', as it is unable
-- to handle 'ShowType' constructors at all.
sTypeError :: HasCallStack => Sing err -> Sing (TypeError err)
sTypeError = typeError . fromSing

$(genDefunSymbols [''ErrorMessage', ''TypeError])

instance SingI (TextSym0 :: Symbol ~> PErrorMessage) where
  sing = singFun1 SText
instance SingI (TyCon1 'Text :: Symbol ~> PErrorMessage) where
  sing = singFun1 SText

instance SingI (ShowTypeSym0 :: t ~> PErrorMessage) where
  sing = singFun1 SShowType
instance SingI (TyCon1 'ShowType :: t ~> PErrorMessage) where
  sing = singFun1 SShowType

instance SingI ((:<>:@#@$) :: PErrorMessage ~> PErrorMessage ~> PErrorMessage) where
  sing = singFun2 (:%<>:)
instance SingI (TyCon2 '(:<>:) :: PErrorMessage ~> PErrorMessage ~> PErrorMessage) where
  sing = singFun2 (:%<>:)
instance SingI x => SingI ((:<>:@#@$$) x :: PErrorMessage ~> PErrorMessage) where
  sing = singFun1 (sing @_ @x :%<>:)
instance SingI x => SingI (TyCon1 ('(:<>:) x) :: PErrorMessage ~> PErrorMessage) where
  sing = singFun1 (sing @_ @x :%<>:)

instance SingI ((:$$:@#@$) :: PErrorMessage ~> PErrorMessage ~> PErrorMessage) where
  sing = singFun2 (:%$$:)
instance SingI (TyCon2 '(:$$:) :: PErrorMessage ~> PErrorMessage ~> PErrorMessage) where
  sing = singFun2 (:%$$:)
instance SingI x => SingI ((:$$:@#@$$) x :: PErrorMessage ~> PErrorMessage) where
  sing = singFun1 (sing @_ @x :%$$:)
instance SingI x => SingI (TyCon1 ('(:$$:) x) :: PErrorMessage ~> PErrorMessage) where
  sing = singFun1 (sing @_ @x :%$$:)

instance SingI TypeErrorSym0 where
  sing = singFun1 sTypeError
