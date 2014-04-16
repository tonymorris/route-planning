{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Geo.Route.Elevation(
  Elevation
, HasElevation(..)
, HasMaybeElevation(..)
, elevationIso
, (<^>)
) where

import Prelude(Show(show), Num, Real, Enum, Integral)
import Data.Eq(Eq)
import Data.Function(id)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Int(Int)
import Data.List((++))
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Control.Lens(Lens', Iso', iso, (?~))

newtype Elevation =
  Elevation
    Int
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

elevationIso ::
  Iso' Int Elevation
elevationIso =
  iso Elevation (\(Elevation s) -> s)

class HasElevation t where
  elevation ::
    Lens' t Elevation

instance HasElevation Elevation where
  elevation =
    id

class HasMaybeElevation t where
  melevation ::
    Lens' t (Maybe Elevation)

instance Gpx Elevation where
  gpx (Elevation e) =
    "<ele>" ++ show e ++ "</ele>"

(<^>) ::
  HasMaybeElevation t =>
  Elevation
  -> t
  -> t
(<^>) =
  (?~) melevation

infixr 5 <^>
