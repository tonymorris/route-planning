module Data.Geo.Route.Name(
  Name
, HasName(..)
, HasNames(..)
, nameIso
, (<.>)
) where

import Prelude(Show)
import Data.Eq(Eq)
import Data.Function(id)
import Data.Ord(Ord)
import Data.List((++))
import Data.String(String, IsString(..))
import Control.Lens(Lens', Traversal', Iso', iso, set)
import Data.Geo.Route.Gpx(Gpx(gpx))

newtype Name =
  Name
    String
  deriving (Eq, Ord, Show)

nameIso ::
  Iso' String Name
nameIso =
  iso Name (\(Name s) -> s)

class HasName t where
  name ::
    Lens' t Name

instance HasName Name where
  name =
    id

class HasNames t where
  names ::
    Traversal' t Name

instance HasNames Name where
  names =
    id

instance IsString Name where
  fromString =
    Name

instance Gpx Name where
  gpx (Name n) =
    "<name>" ++ n ++ "</name>"

(<.>) ::
  HasNames t =>
  Name
  -> t
  -> t
(<.>) =
  set names

infixr 5 <.>
