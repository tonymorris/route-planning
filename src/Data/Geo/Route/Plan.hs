module Data.Geo.Route.Plan(
  Plan
, mkPlan
, mkPlan'
) where

import Prelude(Show)
import Control.Lens(lens)
import Control.Monad(Monad((>>=)))
import Data.Bool((&&))
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Function((.))
import Data.Ord(Ord)
import Data.Maybe(Maybe(Nothing, Just), isNothing)
import Data.String(String)
import Data.Geo.Route.Author(Author, HasMaybeAuthor(mauthor))
import Data.Geo.Route.Copyright(Copyright, HasMaybeCopyright(mcopyright))
import Data.Geo.Route.Description(Description, HasMaybeDescription(mdescription))
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Name(Name, HasMaybeName(mname))
import Data.Geo.Route.Osrm(Osrm(allCoordinates))
import Data.Geo.Route.Track(Track, HasTrack(track), trackPoints)
import Data.Geo.Route.TrackHeader(HasTrackHeader(trackHeader))
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

instance HasMaybeName Plan where
  mname =
    lens (\(Plan n _ _ _ _) -> n) (\(Plan _ d a c t) n -> Plan n d a c t)

instance HasMaybeDescription Plan where
  mdescription =
    lens (\(Plan _ d _ _ _) -> d) (\(Plan n _ a c t) d -> Plan n d a c t)

instance HasMaybeAuthor Plan where
  mauthor =
    lens (\(Plan _ _ a _ _) -> a) (\(Plan n d _ c t) a -> Plan n d a c t)

instance HasMaybeCopyright Plan where
  mcopyright =
    lens (\(Plan _ _ _ c _) -> c) (\(Plan n d a _ t) c -> Plan n d a c t)

instance HasTrack Plan where
  track =
    lens (\(Plan _ _ _ _ t) -> t) (\(Plan n d a c _) t -> Plan n d a c t)

instance HasTrackHeader Plan where
  trackHeader =
    track . trackHeader
