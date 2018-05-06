module Data.Singletons.Single.Fixity where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (NameSpace(..), Quasi(..))
import Data.Singletons.Util
import Data.Singletons.Names
import Language.Haskell.TH.Desugar

singInfixDecl :: DsMonad q => Name -> Fixity -> q DLetDec
singInfixDecl name fixity = do
  mb_ns <- reifyNameSpace name
  pure $ DInfixD fixity
       $ case mb_ns of
           Just TcClsName -> singTyConName name
           Just DataName  -> singDataConName name
           Just VarName   -> singValName name
           -- If we can't find the Name for some odd reason,
           -- fall back to singValName
           Nothing        -> singValName name

singFixityDeclaration :: DsMonad q => Name -> q [DDec]
singFixityDeclaration name = do
  mFixity <- qReifyFixity name
  case mFixity of
    Nothing     -> pure []
    Just fixity -> sequenceA [DLetDec <$> singInfixDecl name fixity]

singFixityDeclarations :: DsMonad q => [Name] -> q [DDec]
singFixityDeclarations = concatMapM trySingFixityDeclaration
  where
    trySingFixityDeclaration name =
      qRecover (return []) (singFixityDeclaration name)
