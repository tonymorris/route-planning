{-# LANGUAGE OverloadedStrings #-}

module Data.Geo.Test where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Geo.Coordinate
import Data.Geo.Route

bourke ::
  Maybe Waypoint
bourke =
  do x <- (-30.088869) ..#.. 145.937757
     return ("City (Medium)" <@> "Bourke, NSW" .<.> x)

warregoHotel ::
  Maybe Waypoint
warregoHotel =
  do x <- (-29.752314) ..#.. 145.425183
     return ("Lodging" <@> "Warrego Hotel" .<.> x)

currawinyaNP ::
  Maybe Waypoint
currawinyaNP =
  do x <- (-28.868504) ..#.. 144.468484
     return ("Forest" <@> "Currawinya NP, QLD" .<.> x)

thargomindah ::
  Maybe Waypoint
thargomindah =
  do x <- (-27.99173) ..#.. 143.819636
     return ("City (Small)" <@> "Thargomindah, QLD" .<.> x)

innamincka ::
  Maybe Waypoint
innamincka =
  do x <- (-27.70761) ..#.. 140.73925
     return ("City (Small)" <@> "Innamincka, SA" .<.> x)

oldStrzleckiNorthEnd ::
  Maybe Waypoint
oldStrzleckiNorthEnd =
  do x <- (-28.08943) ..#.. 140.56706
     return ("Pin, Green" <@> "End of Old Strzelecki Track (north)" .<.> x)

oldStrzleckiSouthStart ::
  Maybe Waypoint
oldStrzleckiSouthStart =
  do x <- (-28.0904) ..#.. 140.5607
     return ("Pin, Green" <@> "Start of Old Strzelecki Track (south)" .<.> x)

mertyMerty ::
  Maybe Waypoint
mertyMerty =
  do x <- (-28.575402) ..#.. 140.290149
     return ("City (Small)" <@> "Merty Merty, SA" .<.> x)

oldStrzeleckiMertyMertyCameronCorner ::
  Maybe Waypoint
oldStrzeleckiMertyMertyCameronCorner =
  do x <- (-28.595917) ..#.. 140.275617
     return ("Pin, Green" <@> "Old Strzelecki Track/Merty Merty,Cameron Corner Road intersection" <~> "Old Strzelecki Track" .<.> x)

mertyMertyCameronCornerStrzelecki ::
  Maybe Waypoint
mertyMertyCameronCornerStrzelecki =
  do x <- (-28.564427) ..#.. 140.186559
     return ("Pin, Green" <@> "Merty Merty,Cameron Corner Road/Strzelecki Track intersection" <~> "Merty Merty,Cameron Corner Road" .<.> x)

lyndhurst ::
  Maybe Waypoint
lyndhurst =
  do x <- (-30.287457) ..#.. 138.3497
     return ("City (Medium)" <@> "Lyndhurst, SA" .<.> x)

borefieldRoadTurn ::
  Maybe Waypoint
borefieldRoadTurn =
  do x <- (-29.593370) ..#.. 137.380702
     return ("Pin, Green" <@> "Borefield Road turn" .<.> x)

olympicDam ::
  Maybe Waypoint
olympicDam =
  do x <- (-30.482769) ..#.. 136.891974
     return ("City (Medium)" <@> "Olympic Dam" .<.> x)

roxbyDowns ::
  Maybe Waypoint
roxbyDowns =
  do x <- (-30.564061) ..#.. 136.895607
     return ("City (Medium)" <@> "Roxby Downs" .<.> x)

purpleDownsTurn ::
  Maybe Waypoint
purpleDownsTurn =
  do x <- (-30.797447) ..#.. 136.914414
     return ("Pin, Green" <@> "Purple Downs turn" .<.> x)

billaKalina ::
  Maybe Waypoint
billaKalina =
  do x <- (-29.917052) ..#.. 136.187145
     return ("Residence" <@> "Billa Kalina" .<.> x)

mountEba ::
  Maybe Waypoint
mountEba =
  do x <- (-30.180817) ..#.. 135.664534
     return ("City (Small)" <@> "Mount Eba" .<.> x)

oldStuartHighwayStuartHighwayIntersection ::
  Maybe Waypoint
oldStuartHighwayStuartHighwayIntersection =
  do x <- (-29.915912) ..#.. 135.154241
     return ("Pin, Green" <@> "Old Stuart/Stuart Highway" .<.> x)

cooberPedy ::
  Maybe Waypoint
cooberPedy =
  do x <- (-29.013368) ..#.. 134.753616
     return ("City (Medium)" <@> "Coober Pedy" .<.> x)

marla ::
  Maybe Waypoint
marla =
  do x <- (-27.305092) ..#.. 133.62192
     return ("City (Small)" <@> "Marla" .<.> x)

trk ::
  Maybe Track
trk =
  let header = mkTrackHeader' "Trans-Australia ride of 2015" "Trans-Australia ride of 2015" "Trans-Australia ride of 2015"
  in (trackHeader .~ header) <$>
     bourke |.|
     warregoHotel |.|
     currawinyaNP |.|
     (-28.291734) <.?> 143.861728 |.|
     thargomindah |.|
     innamincka |.|
     oldStrzleckiNorthEnd |.|
     oldStrzleckiSouthStart |.|
     mertyMerty |.|
     oldStrzeleckiMertyMertyCameronCorner |.|
     mertyMertyCameronCornerStrzelecki |.|
     lyndhurst |.|
     borefieldRoadTurn |.|
     olympicDam |.|
     roxbyDowns |.|
     purpleDownsTurn |.|
     (-30.427641) <.?> 136.399427 |.|
     billaKalina |.|
     mountEba |.|
     (-30.177584) <.?> 135.648081 |.|
     oldStuartHighwayStuartHighwayIntersection |.|
     cooberPedy .|
     marla

plan ::
  Maybe Plan
plan =
  fmap (
    mkPlan'
      "Trans-Australia Ride of 2015"
      "Trans-Australia Ride of 2015"
      (
        mkAuthor'
          "Tony Morris"
          (
            email
              "tonymorris"
              "gmail.com"
          )
          (
            link
              "http://tmorris.net/"
              "Tony Morris"
              "HTTP"
          )
      )
      (
        copyright'
          "Tony Morris"
          "2014"
          "http://en.wikipedia.org/wiki/Beerware"
      )
   ) trk

