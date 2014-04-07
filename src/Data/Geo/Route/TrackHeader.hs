module Data.Geo.Route.TrackHeader(
  TrackHeader
, mkTrackHeader
, mkTrackHeader'
, emptyTrackHeader
, HasTrackHeader(..)
, (<..>)
, (<..^)
) where

import Prelude(Show)
import Control.Applicative((<$>))
import Control.Lens(Lens', (^.), set)
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Function(id)
import Data.Functor(Functor(fmap))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.String(String)
import Data.Traversable(Traversable(traverse))
import Data.Geo.Route.Comment(Comment, HasComments(comments), commentIso)
import Data.Geo.Route.Description(Description, HasDescriptions(descriptions), descriptionIso)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Name(Name, HasNames(names), nameIso)
import Text.Printf(printf)

data TrackHeader =
  TrackHeader
    (Maybe Name)
    (Maybe Comment)
    (Maybe Description)
  deriving (Eq, Ord, Show)

instance HasNames TrackHeader where
  names f (TrackHeader n c d) =
    (\n' -> TrackHeader n' c d) <$> traverse f n

instance HasComments TrackHeader where
  comments f (TrackHeader n c d) =
    (\c' -> TrackHeader n c' d) <$> traverse f c

instance HasDescriptions TrackHeader where
  descriptions f (TrackHeader n c d) =
    TrackHeader n c <$> traverse f d

mkTrackHeader ::
  Maybe Name
  -> Maybe Comment
  -> Maybe Description
  -> TrackHeader
mkTrackHeader =
  TrackHeader

mkTrackHeader' ::
  Name
  -> Comment
  -> Description
  -> TrackHeader
mkTrackHeader' m c d =
  TrackHeader
    (Just m)
    (Just c)
    (Just d)

emptyTrackHeader ::
  TrackHeader
emptyTrackHeader =
  mkTrackHeader
    Nothing
    Nothing
    Nothing

class HasTrackHeader t where
  trackHeader ::
    Lens' t TrackHeader

instance HasTrackHeader TrackHeader where
  trackHeader =
    id

(<..>) ::
  HasTrackHeader t =>
  String
  -> t
  -> t
s <..> t =
  set trackHeader (mkTrackHeader' (s ^. nameIso) (s ^. commentIso) (s ^. descriptionIso)) t

infixr 5 <..>

(<..^) ::
  (Functor f, HasTrackHeader t) =>
  String
  -> f t
  -> f t
s <..^ t =
  fmap (s <..>) t

infixr 5 <..^

instance Gpx TrackHeader where
  gpx (TrackHeader n c d) =
    let gpx' :: (Foldable t, Gpx a) => t a -> String
        gpx' = foldMap gpx
    in printf "%s%s%s" (gpx' n) (gpx' c) (gpx' d)
