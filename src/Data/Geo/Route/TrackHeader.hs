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
import Control.Lens(Lens', lens, (^.), set)
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Function(id)
import Data.Functor(Functor(fmap))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.String(String)
import Data.Geo.Route.Comment(Comment, HasMaybeComment(mcomment), commentIso)
import Data.Geo.Route.Description(Description, HasMaybeDescription(mdescription), descriptionIso)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Name(Name, HasMaybeName(mname), nameIso)
import Text.Printf(printf)

data TrackHeader =
  TrackHeader
    (Maybe Name)
    (Maybe Comment)
    (Maybe Description)
  deriving (Eq, Ord, Show)

instance HasMaybeName TrackHeader where
  mname =
    lens (\(TrackHeader n _ _) -> n) (\(TrackHeader _ c d) n -> TrackHeader n c d)

instance HasMaybeComment TrackHeader where
  mcomment =
    lens (\(TrackHeader _ c _) -> c) (\(TrackHeader n _ d) c -> TrackHeader n c d)

instance HasMaybeDescription TrackHeader where
  mdescription =
    lens (\(TrackHeader _ _ d) -> d) (\(TrackHeader n c _) d -> TrackHeader n c d)

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
