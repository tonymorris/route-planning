module Data.Geo.Route.Author(
  Author
, mkAuthor
, mkAuthor'
, authorEmail
, authorLink
) where

import Prelude(Show)
import Control.Applicative((<$>))
import Control.Lens(Lens', lens)
import Data.Bool((&&))
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Function(id)
import Data.Geo.Route.Email(Email)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Geo.Route.Link(Link)
import Data.Geo.Route.Name(HasNames(names), Name)
import Data.Maybe(Maybe(Nothing, Just), isNothing)
import Data.Ord(Ord)
import Data.String(String)
import Data.Traversable(Traversable(traverse))
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

instance HasNames Author where
  names f (Author n l a) =
    (\n' -> Author n' l a) <$> traverse f n

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