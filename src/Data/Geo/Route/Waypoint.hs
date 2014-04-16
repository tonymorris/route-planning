module Data.Geo.Route.Waypoint(
  Waypoint(..)
, Waypoints
, HasWaypoint(..)
, HasWaypoints(..)
, HasMaybeDateTime(..)
, mkWaypoint
, gpxWaypoint
, (.<.>)
, (~<.>)
, (<%>)
, (<.?>)
, (-.-)
) where


import Prelude(Show(show), Double)
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Function(id, (.))
import Data.Functor(Functor(fmap))
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.String(String)
import Control.Lens(Lens', lens, ( # ), (^.), over, (?~))
import Data.Geo.Coordinate.Latitude(HasLatitude(latitude), fracLatitude)
import Data.Geo.Coordinate.Longitude(HasLongitude(longitude), fracLongitude)
import Data.Geo.Coordinate.Coordinate(Coordinate, HasCoordinate(coordinate), (..#..))
import Data.Geo.Route.Comment(Comment, HasMaybeComment(mcomment), commentIso)
import Data.Geo.Route.Description(Description, HasMaybeDescription(mdescription), descriptionIso)
import Data.Geo.Route.Elevation(Elevation, HasMaybeElevation(melevation))
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Name(Name, HasMaybeName(mname), nameIso)
import Data.Geo.Route.Symbol(Symbol, HasMaybeSymbol(msymbol))
import Text.Printf(printf)
import Text.XML.XSD.DateTime(DateTime)

data Waypoint =
  Waypoint
    Coordinate
    (Maybe Elevation)
    (Maybe DateTime)
    (Maybe Name)
    (Maybe Comment)
    (Maybe Description)
    (Maybe Symbol)
  deriving (Eq, Ord, Show)

type Waypoints =
  [Waypoint]

mkWaypoint ::
  HasCoordinate c =>
  c
  -> Waypoint
mkWaypoint c =
  Waypoint (c ^. coordinate) Nothing Nothing Nothing Nothing Nothing Nothing

instance HasCoordinate Waypoint where
  coordinate =
    lens (\(Waypoint c _ _ _ _ _ _) -> c) (\(Waypoint _ e d n m s y) c -> Waypoint c e d n m s y)

instance HasLatitude Waypoint where
  latitude =
    coordinate . latitude

instance HasLongitude Waypoint where
  longitude =
    coordinate . longitude

instance HasMaybeElevation Waypoint where
  melevation =
    lens (\(Waypoint _ e _ _ _ _ _) -> e) (\(Waypoint c _ d n m s y) e -> Waypoint c e d n m s y)

class HasMaybeDateTime t where
  mdateTime ::
    Lens' t (Maybe DateTime)

instance HasMaybeDateTime Waypoint where
  mdateTime =
    lens (\(Waypoint _ _ d _ _ _ _) -> d) (\(Waypoint c e _ n m s y) d -> Waypoint c e d n m s y)

(<%>) ::
  HasMaybeDateTime t =>
  DateTime
  -> t
  -> t
(<%>) =
  (?~) mdateTime

infixr 5 <%>

instance HasMaybeName Waypoint where
  mname =
    lens (\(Waypoint _ _ _ n _ _ _) -> n) (\(Waypoint c e d _ m s y) n -> Waypoint c e d n m s y)

instance HasMaybeComment Waypoint where
  mcomment =
    lens (\(Waypoint _ _ _ _ m _ _) -> m) (\(Waypoint c e d n _ s y) m -> Waypoint c e d n m s y)

instance HasMaybeDescription Waypoint where
  mdescription =
    lens (\(Waypoint _ _ _ _ _ s _) -> s) (\(Waypoint c e d n m _ y) s -> Waypoint c e d n m s y)

instance HasMaybeSymbol Waypoint where
  msymbol =
    lens (\(Waypoint _ _ _ _ _ _ y) -> y) (\(Waypoint c e d n m s _) y -> Waypoint c e d n m s y)

class HasWaypoint t where
  waypoint ::
    Lens' t Waypoint

instance HasWaypoint Waypoint where
  waypoint =
    id

class HasWaypoints t where
  waypoints ::
    Lens' t Waypoints

gpxWaypoint ::
  (HasMaybeName s, HasMaybeComment s, HasMaybeSymbol s, HasMaybeElevation s, HasMaybeDescription s, HasLatitude s, HasLongitude s, HasMaybeDateTime s) =>
  String
  -> s
  -> String
gpxWaypoint element w =
  let lat = fracLatitude # (w ^. latitude)
      lon = fracLongitude # (w ^. longitude)
      e = w ^. melevation
      d = w ^. mdateTime
      n = w ^. mname
      m = w ^. mcomment
      s = w ^. mdescription
      y = w ^. msymbol
      gpx' :: (Foldable t, Gpx a) => t a -> String
      gpx' = foldMap gpx
  in printf "<%s lat=\"%0.6f\" lon=\"%0.6f\">%s%s%s%s%s%s</%s>" element lat lon (gpx' e) (gpx' d) (gpx' n) (gpx' m) (gpx' s) (gpx' y) element

instance Gpx Waypoint where
  gpx =
    gpxWaypoint "wpt"

(.<.>) ::
  String
  -> Coordinate
  -> Waypoint
s .<.> c =
  let ups i = Just (s ^. i)
  in Waypoint c Nothing Nothing (ups nameIso) (ups commentIso) (ups descriptionIso) Nothing

infixr 5 .<.>

(~<.>) ::
  Name
  -> Coordinate
  -> Waypoint
n ~<.> c =
  Waypoint c Nothing Nothing (Just n) Nothing Nothing Nothing

infixr 5 ~<.>

(<.?>) ::
  Double
  -> Double
  -> Maybe Waypoint
x <.?> y =
  fmap mkWaypoint (x ..#.. y)

infixr 6 <.?>

(-.-) ::
  HasWaypoints t =>
  Maybe Waypoint
  -> t
  -> t
(-.-) Nothing =
  id
(-.-) (Just w) =
  over waypoints (w:)

infixr 6 -.-
