module Data.Geo.Route.Link(
  Link
, mkLink
, linkHref
, linkText
, linkType
) where

import Prelude(Show)
import Control.Lens(Lens', lens)
import Data.Eq(Eq)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Ord(Ord)
import Data.String(String)
import Text.Printf(printf)

data Link =
  Link
    String -- href
    String -- text
    String -- type
  deriving (Eq, Ord, Show)

mkLink ::
  String
  -> String
  -> String
  -> Link
mkLink =
  Link

instance Gpx Link where
  gpx (Link h x t) =
    printf "<link href=\"%s\"><text>%s</text><type>%s</type></link>" h x t

linkHref ::
  Lens' Link String
linkHref =
    lens (\(Link h _ _) -> h) (\(Link _ x t) h -> Link h x t)

linkText ::
  Lens' Link String
linkText =
    lens (\(Link _ x _) -> x) (\(Link h _ t) x -> Link h x t)

linkType ::
  Lens' Link String
linkType =
    lens (\(Link _ _ t) -> t) (\(Link h x _) t -> Link h x t)
