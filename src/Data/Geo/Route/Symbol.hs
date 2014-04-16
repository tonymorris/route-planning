module Data.Geo.Route.Symbol(
  Symbol
, HasSymbol(..)
, HasMaybeSymbol(..)
, symbolIso
, (<@>)
) where

import Prelude(Show)
import Data.Eq(Eq)
import Data.Function(id)
import Data.List((++))
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.String(String, IsString(fromString))
import Control.Lens(Lens', Iso', iso, (?~))
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

class HasMaybeSymbol t where
  msymbol ::
    Lens' t (Maybe Symbol)

instance IsString Symbol where
  fromString =
    Symbol

instance Gpx Symbol where
  gpx (Symbol s) =
    "<sym>" ++ s ++ "</sym>"

(<@>) ::
  HasMaybeSymbol t =>
  Symbol
  -> t
  -> t
(<@>) =
  (?~) msymbol

infixr 5 <@>
