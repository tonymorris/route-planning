module Data.Geo.Route.Plan(
  Plan
, mkPlan
, mkPlan'
) where

import Prelude(Show)
import Control.Applicative((<$>))
import Control.Monad(Monad((>>=)))
import Data.Bool((&&))
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Ord(Ord)
import Data.Maybe(Maybe(Nothing, Just), isNothing)
import Data.String(String)
import Data.Traversable(Traversable(traverse))
import Data.Geo.Route.Author(Author)
import Data.Geo.Route.Copyright(Copyright)
import Data.Geo.Route.Description(Description, HasDescriptions(descriptions))
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Name(Name, HasNames(names))
import Data.Geo.Route.Osrm(Osrm(allCoordinates))
import Data.Geo.Route.Track(Track, trackPoints)
import Data.Geo.Route.Waypoint(gpxWaypoint)
import Text.Printf(printf)

data Plan =
  Plan
    (Maybe Name) -- name
    (Maybe Description) -- description
    (Maybe Author) -- author
    (Maybe Copyright) -- copyright
    Track
  deriving (Eq, Ord, Show)

mkPlan ::
  Track
  -> Plan
mkPlan =
  Plan Nothing Nothing Nothing Nothing

mkPlan' ::
  Name
  -> Description
  -> Author
  -> Copyright
  -> Track
  -> Plan
mkPlan' n d a c =
  Plan (Just n) (Just d) (Just a) (Just c)

instance Gpx Plan where
  gpx (Plan n d a c t) =
    let gpx' :: (Foldable t, Gpx a) => t a -> String
        gpx' = foldMap gpx
        metadata =
          if isNothing n && isNothing d && isNothing a && isNothing c
          then ""
          else printf "%s%s%s%s%s%s"
                 "<metadata>"
                 (gpx' n)
                 (gpx' d)
                 (gpx' a)
                 (gpx' c)
                 "</metadata>"
        wpt =
          trackPoints t >>= gpx
        rte ::
          String
        rte =
          printf "<rte>%s</rte>" (trackPoints t >>= gpxWaypoint "rtept")
        trk =
          gpx t
    in printf "%s%s%s%s%s%s"
              "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?><gpx xmlns=\"http://www.topografix.com/GPX/1/1\" creator=\"Data.Geo.Route\" version=\"1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.garmin.com/xmlschemas/GpxExtensions/v3 http://www.garmin.com/xmlschemas/GpxExtensionsv3.xsd http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd\">"
              metadata
              wpt
              rte
              trk
              "</gpx>"

instance Osrm Plan where
  allCoordinates (Plan _ _ _ _ t) =
    allCoordinates t

instance HasNames Plan where
  names f (Plan n d a c t) =
    (\n' -> Plan n' d a c t) <$> traverse f n

instance HasDescriptions Plan where
  descriptions f (Plan n d a c t) =
    (\d' -> Plan n d' a c t) <$> traverse f d
