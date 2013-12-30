module Data.Geo.Route.Comment(
  Comment
, HasComment(..)
, HasComments(..)
, commentIso
, (<!>)
) where

import Prelude(Show)
import Data.Eq(Eq)
import Data.Function(id)
import Data.List((++))
import Data.Ord(Ord)
import Data.String(String, IsString(fromString))
import Control.Lens(Lens', Iso', Traversal', iso, set)
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

class HasComments t where
  comments ::
    Traversal' t Comment

instance HasComments Comment where
  comments =
    id

instance IsString Comment where
  fromString =
    Comment

instance Gpx Comment where
  gpx (Comment c) =
    "<cmt>" ++ c ++ "</cmt>"

(<!>) ::
  HasComments t =>
  Comment
  -> t
  -> t
(<!>) =
  set comments

infixr 5 <!>
