module Data.Singletons.Single.Fixity where

import Prelude hiding ( exp )
import Language.Haskell.TH hiding ( cxt )
import Language.Haskell.TH.Syntax (Quasi(..))
import Data.Singletons.Util
import Data.Singletons.Names
import Language.Haskell.TH.Desugar

singInfixDecl :: Fixity -> Name -> DLetDec
singInfixDecl fixity name
  | isUpcase name =
    -- is it a tycon name or a datacon name??
    -- it *must* be a datacon name, because symbolic tycons
    -- can't be promoted. This is terrible.
    DInfixD fixity (singDataConName name)
  | otherwise = DInfixD fixity (singValName name)

singFixityDeclaration :: DsMonad q => Name -> q [DDec]
singFixityDeclaration name = do
  mFixity <- qReifyFixity name
  return $ case mFixity of
    Nothing     -> []
    Just fixity -> [DLetDec $ singInfixDecl fixity name]

singFixityDeclarations :: DsMonad q => [Name] -> q [DDec]
singFixityDeclarations = concatMapM trySingFixityDeclaration
  where
    trySingFixityDeclaration name =
      qRecover (return []) (singFixityDeclaration name)
