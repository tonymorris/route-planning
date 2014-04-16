module Data.Geo.Route.Author(
  Author
, mkAuthor
, mkAuthor'
, authorEmail
, authorLink
) where

import Prelude(Show)
import Control.Lens(Lens', lens)
import Data.Bool((&&))
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Function(id)
import Data.Geo.Route.Email(Email)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Link(Link)
import Data.Geo.Route.Name(HasMaybeName(mname), Name)
import Data.Maybe(Maybe(Nothing, Just), isNothing)
import Data.Ord(Ord)
import Data.String(String)
import Text.Printf(printf)

data Author =
  Author
    (Maybe Name) -- name
    (Maybe Email) -- email
    (Maybe Link) -- link
  deriving (Eq, Ord, Show)

mkAuthor ::
  Author
mkAuthor =
  Author Nothing Nothing Nothing

mkAuthor' ::
  Name
  -> Email
  -> Link
  -> Author
mkAuthor' e l a =
  Author (Just e) (Just l) (Just a)

instance HasMaybeName Author where
  mname =
    lens (\(Author n _ _) -> n) (\(Author _ e l) n -> Author n e l)

authorEmail ::
  Lens' Author (Maybe Email)
authorEmail =
    lens (\(Author _ e _) -> e) (\(Author n _ l) e -> Author n e l)

authorLink ::
  Lens' Author (Maybe Link)
authorLink =
    lens (\(Author _ _ l) -> l) (\(Author n e _) l -> Author n e l)

instance Gpx Author where
  gpx (Author n e l) =
    let gpx' :: (Foldable t, Gpx a) => t a -> String
        gpx' = foldMap gpx
    in if isNothing n && isNothing e && isNothing l
       then ""
       else printf "%s%s%s%s%s"
              "<author>"
              (gpx' n)
              (gpx' e)
              (gpx' l)
              "</author>"

class HasAuthor t where
  author ::
    Lens' t Author

instance HasAuthor Author where
  author =
    id