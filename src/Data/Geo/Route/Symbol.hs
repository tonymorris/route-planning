module Data.Geo.Route.Symbol(
  Symbol
, HasSymbol(..)
, HasSymbols(..)
, symbolIso
, (<@>)
) where

import Prelude(Show)
import Data.Eq(Eq)
import Data.Function(id)
import Data.List((++))
import Data.Ord(Ord)
import Data.String(String, IsString(fromString))
import Control.Lens(Lens', Iso', Traversal', iso, set)
import Data.Geo.Route.Gpx(Gpx(gpx))

newtype Symbol =
  Symbol
    String
  deriving (Eq, Ord, Show)

symbolIso ::
  Iso' String Symbol
symbolIso =
  iso Symbol (\(Symbol s) -> s)

class HasSymbol t where
  symbol ::
    Lens' t Symbol

instance HasSymbol Symbol where
  symbol =
    id

class HasSymbols t where
  symbols ::
    Traversal' t Symbol

instance HasSymbols Symbol where
  symbols =
    id

instance IsString Symbol where
  fromString =
    Symbol

instance Gpx Symbol where
  gpx (Symbol s) =
    "<sym>" ++ s ++ "</sym>"

(<@>) ::
  HasSymbols t =>
  Symbol
  -> t
  -> t
(<@>) =
  set symbols

infixr 5 <@>
