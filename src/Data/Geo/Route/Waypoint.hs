module Data.Geo.Route.Waypoint(
  Waypoint
, Waypoints
, HasWaypoint(..)
, HasWaypoints(..)
, HasDateTimes(..)
, mkWaypoint
, gpxWaypoint
, (.<.>)
, (~<.>)
, (<%>)
, (<.?>)
, (-.-)
) where


import Prelude(Show(show), Double)
import Control.Applicative((<$>))
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Function(id, (.))
import Data.Functor(Functor(fmap))
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.String(String)
import Data.Traversable(traverse)
import Control.Lens(Lens', Traversal', lens, (#), (^.), over, firstOf, set)
import Data.Geo.Coordinate.Latitude(HasLatitude(latitude), fracLatitude)
import Data.Geo.Coordinate.Longitude(HasLongitude(longitude), fracLongitude)
import Data.Geo.Coordinate.Coordinate(Coordinate, HasCoordinate(coordinate), (..#..))
import Data.Geo.Route.Comment(Comment, HasComments(comments), commentIso)
import Data.Geo.Route.Description(Description, HasDescriptions(descriptions), descriptionIso)
import Data.Geo.Route.Elevation(Elevation, HasElevations(elevations))
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Name(Name, HasNames(names), nameIso)
import Data.Geo.Route.Symbol(Symbol, HasSymbols(symbols))
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

instance HasElevations Waypoint where
  elevations f (Waypoint c e d n m s y) =
    (\e' -> Waypoint c e' d n m s y) <$> traverse f e

class HasDateTimes t where
  dateTimes ::
    Traversal' t DateTime

instance HasDateTimes DateTime where
  dateTimes =
    id

instance HasDateTimes Waypoint where
  dateTimes f (Waypoint c e d n m s y) =
    (\d' -> Waypoint c e d' n m s y) <$> traverse f d

(<%>) ::
  HasDateTimes t =>
  DateTime
  -> t
  -> t
(<%>) =
  set dateTimes

infixr 5 <%>

instance HasNames Waypoint where
  names f (Waypoint c e d n m s y) =
    (\n' -> Waypoint c e d n' m s y) <$> traverse f n

instance HasComments Waypoint where
  comments f (Waypoint c e d n m s y) =
    (\m' -> Waypoint c e d n m' s y) <$> traverse f m

instance HasDescriptions Waypoint where
  descriptions f (Waypoint c e d n m s y) =
    (\s' -> Waypoint c e d n m s' y) <$> traverse f s

instance HasSymbols Waypoint where
  symbols f (Waypoint c e d n m s y) =
    (\y' -> Waypoint c e d n m s y') <$> traverse f y

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
  (HasNames s, HasComments s, HasSymbols s, HasElevations s, HasDescriptions s, HasLatitude s, HasLongitude s,HasDateTimes s) =>
  String
  -> s
  -> String
gpxWaypoint element w =
  let lat = fracLatitude # (w ^. latitude)
      lon = fracLongitude # (w ^. longitude)
      e = firstOf elevations w
      d = firstOf dateTimes w
      n = firstOf names w
      m = firstOf comments w
      s = firstOf descriptions w
      y = firstOf symbols w
      gpx' r = foldMap gpx r
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
