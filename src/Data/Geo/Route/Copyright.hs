module Data.Geo.Route.Copyright(
  Copyright
, mkCopyright
, mkCopyright'
, copyrightAuthor
, copyrightYear
, copyrightLicense
, HasCopyright(..)
, HasMaybeCopyright(..)
) where

import Prelude(Show)
import Control.Lens(Lens', lens)
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Function(id)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Data.List((++))
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

mkCopyright ::
  String
  -> Copyright
mkCopyright a =
  Copyright a Nothing Nothing

mkCopyright' ::
  String
  -> String
  -> String
  -> Copyright
mkCopyright' a y l =
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
    printf "<copyright author=\"%s\">%s%s</copyright>" a (foldMap (\y' -> "<year>" ++ y' ++ "</year>") y) (foldMap (\l' -> "<license>" ++ l' ++ "</license>") l)

class HasCopyright t where
  copyright ::
    Lens' t Copyright

instance HasCopyright Copyright where
  copyright =
    id

class HasMaybeCopyright t where
  mcopyright ::
    Lens' t (Maybe Copyright)
