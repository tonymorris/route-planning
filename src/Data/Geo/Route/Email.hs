module Data.Geo.Route.Email(
  Email
, mkEmail
, emailId
, emailDomain
, HasEmail(..)
, HasMaybeEmail(..)
) where

import Prelude(Show)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Control.Lens(Lens', lens)
import Data.Function(id)
import Data.Maybe(Maybe)
import Data.String(String)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Text.Printf(printf)

data Email =
  Email
    String -- id
    String -- domain
  deriving (Eq, Ord, Show)

mkEmail ::
  String
  -> String
  -> Email
mkEmail =
  Email

emailId ::
  Lens' Email String
emailId =
  lens (\(Email i _) -> i) (\(Email _ d) i -> Email i d)

emailDomain ::
  Lens' Email String
emailDomain =
  lens (\(Email _ d) -> d) (\(Email i _) d -> Email i d)

instance Gpx Email where
  gpx (Email i d) =
    printf "<email id=\"%s\" domain=\"%s\"/>" i d

class HasEmail t where
  email ::
    Lens' t Email

instance HasEmail Email where
  email =
    id

class HasMaybeEmail t where
  memail ::
    Lens' t (Maybe Email)
