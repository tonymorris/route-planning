module Data.Geo.Route.Osrm(
  Osrm(..)
) where

import Prelude(Show(show))
import Data.Eq(Eq((==)))
import Data.Function(id, (.))
import Data.Functor(Functor(fmap))
import Data.List((++), intercalate)
import Data.String(String)
import Control.Lens(( # ), (^.))
import Data.Geo.Coordinate.Latitude(HasLatitude(latitude), latitude, fracLatitude)
import Data.Geo.Coordinate.Longitude(HasLongitude(longitude), longitude, fracLongitude)
import Data.Geo.Coordinate.Coordinate(Coordinate, HasCoordinate(coordinate))
import Text.Printf(printf)

class Osrm o where
  allCoordinates ::
    o
    -> [Coordinate]
  osrm ::
    o
    -> String
  osrm =
    let removeAdjacentDuplicates ::
          Eq a =>
          [a]
          -> [a]
        removeAdjacentDuplicates [] =
          []
        removeAdjacentDuplicates [h] =
          [h]
        removeAdjacentDuplicates (h:x:xs) =
          (if h == x then id else (h:)) (removeAdjacentDuplicates (x:xs))
    in ("http://map.project-osrm.org/?" ++) .
       intercalate "&" .
       fmap (\c -> let c' = c ^. coordinate
                   in printf "loc=%0.6f,%0.6f" (fracLatitude # (c' ^. latitude)) (fracLongitude # (c' ^. longitude))) .
       removeAdjacentDuplicates .
       allCoordinates
