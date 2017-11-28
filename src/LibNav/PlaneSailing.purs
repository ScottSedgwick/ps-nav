module LibNav.PlaneSailing where

import Prelude ((<), ($), (-), (>), (/), (+), (*))
import Math (abs, atan, cos, pi, tan)

import LibNav.GreatCircle (gcQuadAdjust)
import LibNav.Types (Degrees(..), Posn(..), Quadrant(..), NMiles, degToRad, radToDeg, tau)

planeQuadrant :: Posn -> Posn -> Quadrant
planeQuadrant (Posn p1) (Posn p2)
  | p1.lat < p2.lat = -- N
    if p1.lon < p2.lon
    then if (p2.lon - p1.lon) > Deg 180.0 then NW else NE
    else if (p1.lon - p2.lon) > Deg 180.0 then NE else NW
  | p1.lon < p2.lon = if (p2.lon - p1.lon) > Deg 180.0 then SW else SE
  | (p1.lon - p2.lon) > Deg 180.0 = SE
  | true = SW

planeCourse :: Posn -> Posn -> Degrees
planeCourse (Posn p1) (Posn p2) = gcQuadAdjust (planeQuadrant (Posn p1) (Posn p2)) (radToDeg rawCrs)
    where
        rlat1      = degToRad p1.lat
        rlat2      = degToRad p2.lat
        rlon1      = degToRad p1.lon
        rlon2      = degToRad p2.lon
        dLat       = abs $ rlat1 - rlat2
        dl         = abs $ rlon1 - rlon2
        dLon       = abs $ if dl > pi then tau - dl else dl
        meanLat    = (rlat1 + rlat2) / 2.0
        dep        = dLon * cos meanLat
        rawCrs     = atan $ dep / dLat


planeDistance :: Posn -> Posn -> NMiles
planeDistance (Posn p1) (Posn p2) = abs $ 60.0 * dLat / c
    where
        (Deg dLat) = p1.lat - p2.lat
        c = cos $ degToRad $ planeCourse (Posn p1) (Posn p2)

-- distance :: Knots -> Hours -> NMiles
-- distance = (*)

-- speed :: NMiles -> Hours -> Knots
-- speed = (/)

-- timeTaken :: NMiles -> Knots -> Hours
-- timeTaken = (/)

dcos :: Degrees -> Number
dcos a = cos $ degToRad a

dtan :: Degrees -> Number
dtan a = tan $ degToRad a

planeDR :: Posn -> Degrees -> NMiles -> Posn
planeDR (Posn p) c d = Posn { lat: fLat, lon: fLon }
    where
        dLat      = d * dcos c / 60.0
        fLat      = p.lat + (Deg dLat)
        dep       = dLat * dtan c
        meanLat   = p.lat + Deg (dLat / 2.0)
        dLon      = dep / dcos meanLat
        lon       = p.lon + (Deg dLon)
        fLon      = if lon > (Deg 180.0) then lon - (Deg 360.0) else lon