{-# LANGUAGE OverloadedStrings #-}
module T536 where

import           Data.Singletons.TH         (genSingletons, showSingInstances,
                                             singDecideInstances, singletons)
import           Data.Singletons.TH.Options (defaultOptions,
                                             defunctionalizedName,
                                             promotedDataTypeOrConName,
                                             withOptions)
import           Data.String                (fromString)
import           Data.String.Singletons     (FromString, sFromString)
import           Data.Text                  (Text)
import           GHC.TypeLits.Singletons    (Symbol)
import           Language.Haskell.TH        (Name)

-- Term-level
newtype Message = MkMessage Text
-- Type-level
newtype PMessage = PMkMessage Symbol

$(let customPromote :: Name -> Name
      customPromote n
        | n == ''Message  = ''PMessage
        | n == 'MkMessage = 'PMkMessage
        | n == ''Text     = ''Symbol
        | otherwise       = promotedDataTypeOrConName defaultOptions n

      customDefun :: Name -> Int -> Name
      customDefun n sat = defunctionalizedName defaultOptions (customPromote n) sat in

  withOptions defaultOptions{ promotedDataTypeOrConName = customPromote
                            , defunctionalizedName      = customDefun
                            } $ do
    decs1 <- genSingletons [''Message]
    decs2 <- singletons [d|
               hello :: Message
               hello = MkMessage "hello"
               |]
    decs3 <- singDecideInstances [''Message]
    decs4 <- showSingInstances [''Message]
    return $ decs1 ++ decs2 ++ decs3 ++ decs4)
