module Data.Geo.Route.Name(
  Name
, HasName(..)
, HasMaybeName(..)
, nameIso
, (<.>)
) where

import Prelude(Show)
import Data.Eq(Eq)
import Data.Function(id)
import Data.Ord(Ord)
import Data.List((++))
import Data.Maybe(Maybe)
import Data.String(String, IsString(..))
import Control.Lens(Lens', Iso', iso, (?~))
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

class HasMaybeName t where
  mname ::
    Lens' t (Maybe Name)

instance IsString Name where
  fromString =
    Name

instance Gpx Name where
  gpx (Name n) =
    "<name>" ++ n ++ "</name>"

(<.>) ::
  HasMaybeName t =>
  Name
  -> t
  -> t
(<.>) =
  (?~) mname

infixr 5 <.>
