module Data.Geo.Route.Gpx(
  Gpx(..)
) where

import Control.Lens(( # ))
import Data.List((++))
import Data.String(String)
import Data.Text(unpack)
import Text.XML.XSD.DateTime(DateTime, dateTime)

class Gpx x where
  gpx ::
    x
    -> String

instance Gpx DateTime where
  gpx d =
    "<time>" ++ unpack (dateTime # d) ++ "</time>"
