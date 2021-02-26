module T450 where

import Data.Maybe
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Data.Text (Text)
import Language.Haskell.TH (Name)
import Prelude.Singletons

newtype  Message =  MkMessage Text
newtype PMessage = PMkMessage Symbol

newtype  Function a b =  MkFunction (a -> b)
newtype PFunction a b = PMkFunction (a ~> b)

$(do let customPromote :: [(Name, Name)] -> Name -> Name
         customPromote customs n = fromMaybe n $ lookup n customs

         customOptions :: [(Name, Name)] -> Options
         customOptions customs =
           defaultOptions{ promotedDataTypeOrConName = \n ->
                             promotedDataTypeOrConName defaultOptions
                               (customPromote customs n)
                         , defunctionalizedName = \n sat ->
                             defunctionalizedName defaultOptions
                               (customPromote customs n) sat
                         }

     messageDecs <-
       withOptions (customOptions [ (''Message, ''PMessage)
                                  , ('MkMessage, 'PMkMessage)
                                  , (''Text, ''Symbol)
                                  ]) $ do
         messageDecs1 <- genSingletons [''Message]
         messageDecs2 <- singletons [d|
           appendMessage :: Message -> Message -> Message
           appendMessage (MkMessage (x :: Text)) (MkMessage (y :: Text)) =
             MkMessage (x <> y :: Text)
           |]
         pure $ messageDecs1 ++ messageDecs2

     functionDecs <-
       withOptions (customOptions [ (''Function, ''PFunction)
                                  , ('MkFunction, 'PMkFunction)
                                  ]) $ do
         functionDecs1 <- genSingletons [''Function]
         functionDecs2 <- singletons [d|
           composeFunction :: Function b c -> Function a b -> Function a c
           composeFunction (MkFunction (f :: b -> c)) (MkFunction (g :: a -> b)) =
             MkFunction (f . g :: a -> c)
           |]
         pure $ functionDecs1 ++ functionDecs2
     pure $ messageDecs ++ functionDecs)
