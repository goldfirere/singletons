module T487 where

import Data.Singletons.Base.TH
import GHC.TypeLits.Singletons
import Text.Show.Singletons

$(singletonsOnly [d|
  exprEx :: Char
  exprEx = 'a'

  consEx :: Char -> Symbol
  consEx x = consSymbol x "bc"

  unconsEx :: Maybe (Char, Symbol)
  unconsEx = unconsSymbol "abc"

  natToCharEx :: Char
  natToCharEx = natToChar 97

  charToNatEx :: Natural
  charToNatEx = charToNat 'a'
  |])

showCharTest :: Show_ ExprEx :~: "'a'"
showCharTest = Refl

showSymbolTest :: Show_ "Hello, World!" :~: "\"Hello, World!\""
showSymbolTest = Refl

consSymbolTest :: ConsEx 'a' :~: "abc"
consSymbolTest = Refl

unconsSymbolTest :: UnconsEx :~: Just '( 'a', "bc" )
unconsSymbolTest = Refl

natToCharTest :: NatToCharEx :~: 'a'
natToCharTest = Refl

charToNatTest :: CharToNatEx :~: 97
charToNatTest = Refl
