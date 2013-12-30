{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Geo.Route.Distance(
  Distance
, HasDistance(..)
, HasMaybeDistance(..)
, distanceIso
) where

import Prelude(Show, Num, Real, Enum, Integral)
import Data.Eq(Eq)
import Data.Function(id)
import Data.Int(Int)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Control.Lens(Lens', Iso', iso)

newtype Distance =
  Distance
    Int
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

distanceIso ::
  Iso' Int Distance
distanceIso =
  iso Distance (\(Distance s) -> s)

class HasDistance t where
  distance ::
    Lens' t Distance

class HasMaybeDistance t where
  maybeDistance ::
    Lens' t (Maybe Distance)

instance HasDistance Distance where
  distance =
    id
