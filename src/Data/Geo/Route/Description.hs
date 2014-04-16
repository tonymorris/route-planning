module Data.Geo.Route.Description(
  Description
, HasDescription(..)
, HasMaybeDescription(..)
, descriptionIso
, (<~>)
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

newtype Description =
  Description
    String
  deriving (Eq, Ord, Show)

descriptionIso ::
  Iso' String Description
descriptionIso =
  iso Description (\(Description s) -> s)

class HasDescription t where
  description ::
    Lens' t Description

instance HasDescription Description where
  description =
    id

class HasMaybeDescription t where
  mdescription ::
    Lens' t (Maybe Description)

instance IsString Description where
  fromString =
    Description

instance Gpx Description where
  gpx (Description d) =
    "<desc>" ++ d ++ "</desc>"

(<~>) ::
  HasMaybeDescription t =>
  Description
  -> t
  -> t
(<~>) =
  (?~) mdescription

infixr 5 <~>
