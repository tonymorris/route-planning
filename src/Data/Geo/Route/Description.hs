module Data.Geo.Route.Description(
  Description
, HasDescription(..)
, HasDescriptions(..)
, descriptionIso
, (<~>)
) where

import Prelude(Show)
import Data.Eq(Eq)
import Data.Function(id)
import Data.List((++))
import Data.Ord(Ord)
import Data.String(String, IsString(fromString))
import Control.Lens(Lens', Traversal', Iso', iso, set)
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

class HasDescriptions t where
  descriptions ::
    Traversal' t Description

instance HasDescriptions Description where
  descriptions =
    id

instance IsString Description where
  fromString =
    Description

instance Gpx Description where
  gpx (Description d) =
    "<desc>" ++ d ++ "</desc>"

(<~>) ::
  HasDescriptions t =>
  Description
  -> t
  -> t
(<~>) =
  set descriptions

infixr 5 <~>