module Data.Geo.Route.Email(
  Email
, email
, emailId
, emailDomain
) where

import Prelude(Show)
import Data.Geo.Route.Gpx(Gpx(gpx))
import Control.Lens(Lens', lens)
import Data.String(String)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Text.Printf(printf)

data Email =
  Email
    String -- id
    String -- domain
  deriving (Eq, Ord, Show)

email ::
  String
  -> String
  -> Email
email =
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
