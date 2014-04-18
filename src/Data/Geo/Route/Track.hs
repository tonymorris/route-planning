module Data.Geo.Route.Track(
  Track
, HasTrack(..)
, (-|)
, (.|)
, (--|)
, (..|)
, (|-|)
, (|.|)
, (|--|)
, (|..|)
, trackPoints
) where

import Prelude(Show)
import Control.Lens(Lens', (^.), lens)
import Control.Monad(Monad(return, (>>=)))
import Data.Eq(Eq)
import Data.Function(id)
import Data.Functor.Apply(Apply, liftF2)
import Data.Ord(Ord)
import Data.String(String)
import Data.Geo.Coordinate(HasCoordinate(coordinate))
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Osrm(Osrm(allCoordinates))
import Data.Geo.Route.TrackHeader(TrackHeader, HasTrackHeader(trackHeader), emptyTrackHeader)
import Data.Geo.Route.Waypoint(Waypoint, gpxWaypoint)
import Data.List(concat)
import Text.Printf(printf)

data Track =
  Track TrackHeader [[Waypoint]]
  deriving (Eq, Ord, Show)

(-|) ::
  Waypoint
  -> Waypoint
  -> Track
w1 -| w2 =
  Track emptyTrackHeader [[w1, w2]]

infixr 5 -|

(.|) ::
  Apply f =>
  f Waypoint
  -> f Waypoint
  -> f Track
(.|)=
  liftF2 (-|)

infixr 5 .|

(--|) ::
  Waypoint
  -> Waypoint
  -> Track
w1 --| w2 =
  Track emptyTrackHeader [[w1], [w2]]

infixr 5 --|

(..|) ::
  Apply f =>
  f Waypoint
  -> f Waypoint
  -> f Track
(..|)=
  liftF2 (--|)

infixr 5 ..|

(|-|) ::
  Waypoint
  -> Track
  -> Track
w |-| Track r [] =
  Track r [[w]]
w |-| Track r (h:t) =
  Track r ((w:h):t)

infixr 5 |-|

(|.|) ::
  Apply f =>
  f Waypoint
  -> f Track
  -> f Track
(|.|) =
  liftF2 (|-|)

infixr 5 |.|

(|--|) ::
  Waypoint
  -> Track
  -> Track
w |--| Track r t =
  Track r ([w]:t)

infixr 5 |--|

(|..|) ::
  Apply f =>
  f Waypoint
  -> f Track
  -> f Track
(|..|) =
  liftF2 (|--|)

infixr 5 |..|

trackPoints ::
  Track
  -> [Waypoint]
trackPoints (Track _ t) =
  concat t

class HasTrack t where
  track ::
    Lens' t Track

instance HasTrack Track where
  track =
    id

instance HasTrackHeader Track where
  trackHeader =
    lens (\(Track r _) -> r) (\(Track _ t) r -> Track r t)


instance Osrm Track where
  allCoordinates (Track _ t) =
    do w <- t
       x <- w
       return (x ^. coordinate)

instance Gpx Track where
  gpx (Track r t) =
    printf "<trk>%s%s</trk>" (gpx r) (t >>= \listways ->
                                    printf "<trkseg>%s</trkseg>" (listways >>= gpxWaypoint "trkpt") :: String)
