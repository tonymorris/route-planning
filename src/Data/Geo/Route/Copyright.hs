module Data.Geo.Route.Copyright(
  Copyright
, copyright
, copyright'
, copyrightAuthor
, copyrightYear
, copyrightLicense
) where

import Prelude(Show)
import Control.Lens(Lens', lens)
import Data.Eq(Eq)
import Data.Foldable(Foldable(fold))
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.String(String)
import Text.Printf(printf)

data Copyright =
  Copyright
    String -- author
    (Maybe String) -- year
    (Maybe String) -- license
  deriving (Eq, Ord, Show)

copyright ::
  String
  -> Copyright
copyright a =
  Copyright a Nothing Nothing

copyright' ::
  String
  -> String
  -> String
  -> Copyright
copyright' a y l =
  Copyright a (Just y) (Just l)

copyrightAuthor ::
  Lens' Copyright String
copyrightAuthor =
    lens (\(Copyright a _ _) -> a) (\(Copyright _ y l) a -> Copyright a y l)

copyrightYear ::
  Lens' Copyright (Maybe String)
copyrightYear =
    lens (\(Copyright _ y _) -> y) (\(Copyright a _ l) y -> Copyright a y l)

copyrightLicense ::
  Lens' Copyright (Maybe String)
copyrightLicense =
    lens (\(Copyright _ _ l) -> l) (\(Copyright a y _) l -> Copyright a y l)

instance Gpx Copyright where
  gpx (Copyright a y l) =
    printf "<copyright author=\"%s\">%s%s</copyright>" a (fold y) (fold l)
