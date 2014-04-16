module Data.Geo.Route.Comment(
  Comment
, HasComment(..)
, HasMaybeComment(..)
, commentIso
, (<!>)
) where

import Prelude(Show)
import Data.Eq(Eq)
import Data.Function(id)
import Data.List((++))
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.String(String, IsString(fromString))
import Control.Lens(Lens', Iso', iso, (?~))
import Data.Geo.Route.Gpx(Gpx(gpx))

newtype Comment =
  Comment
    String
  deriving (Eq, Ord, Show)

commentIso ::
  Iso' String Comment
commentIso =
  iso Comment (\(Comment s) -> s)

class HasComment t where
  comment ::
    Lens' t Comment

instance HasComment Comment where
  comment =
    id

class HasMaybeComment t where
  mcomment ::
    Lens' t (Maybe Comment)

instance IsString Comment where
  fromString =
    Comment

instance Gpx Comment where
  gpx (Comment c) =
    "<cmt>" ++ c ++ "</cmt>"

(<!>) ::
  HasMaybeComment t =>
  Comment
  -> t
  -> t
(<!>) =
  (?~) mcomment

infixr 5 <!>
