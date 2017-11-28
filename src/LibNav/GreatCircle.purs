module LibNav.GreatCircle where

-- The math and worked examples for this unit taken from:
-- http://shipofficer.com/so/wp-content/uploads/2015/02/10.-Great-Circle-Sailing.pdf
-- A copy of this PDF is in the docs folder.

import Prelude (negate, otherwise, ($), (&&), (*), (+), (-), (/), (<), (==), (>), (||))
      
import LibNav.Types (Degrees(..), EW(..), NMiles, Posn(..), Quadrant(..), dacos, dcos, dsin)
import Math (abs)

gcDistance :: Posn -> Posn -> NMiles
gcDistance posnA posnB = 60.0 * d
    where
        (Deg d) = gcArc posnA posnB

gcArc :: Posn -> Posn -> Degrees
gcArc (Posn posnA) (Posn posnB) = 
    dacos $ dsin (posnA.lat) * dsin (posnB.lat) +
            dcos (posnA.lat) * dcos (posnB.lat) * dcos (posnB.lon - posnA.lon)

getDir :: Degrees -> Degrees -> EW
getDir (Deg a) (Deg b) 
    | abs (b - a) > 180.0 = if a < b then W else E
    | a < b               = E
    | otherwise           = W

gcInitQuadrant :: Posn -> Posn -> Quadrant
gcInitQuadrant (Posn a) (Posn b) =
  if a.lat > Deg 0.0 
    then if d == E then NE else NW
    else if d == E then SE else SW
  where d = getDir a.lon b.lon

gcFinalQuadrant :: Posn -> Posn -> Quadrant
gcFinalQuadrant (Posn a) (Posn b) =
  if a.lat > Deg 0.0
    then if d == E then SE else SW
    else if d == E then NE else NW
  where d = getDir (a.lon) (b.lon)

gcQuadAdjust :: Quadrant -> Degrees -> Degrees
gcQuadAdjust q (Deg r) = Deg $
    case q of
        NE -> r
        SE -> 180.0 - r
        NW -> 360.0 - r
        SW -> 180.0 + r
        
gcCourse :: (Degrees -> Degrees -> Degrees -> Degrees) -> (Posn -> Posn -> Quadrant) -> Posn -> Posn -> Degrees
gcCourse f g (Posn posnA) (Posn posnB) = gcQuadAdjust quad rawCrs
    where
        dAB     = gcArc (Posn posnA) (Posn posnB)
        (Deg latAraw) = posnA.lat
        (Deg latBraw) = posnB.lat
        latA    = abs latAraw
        latB    = (if ((latAraw > 0.0) && (latBraw < 0.0)) || ((latAraw < 0.0) && (latBraw > 0.0)) then (-1.0) else 1.0) * abs latBraw
        rawCrs  = f (Deg latA) (Deg latB) dAB
        quad    = g (Posn posnA) (Posn posnB)
        
gcInitCourse :: Posn -> Posn -> Degrees
gcInitCourse = gcCourse f gcInitQuadrant
    where 
        f latA latB dAB = dacos $ (dsin latB - dsin latA * dcos dAB) / (dcos latA * dsin dAB)
    
gcFinalCourse :: Posn -> Posn -> Degrees
gcFinalCourse = gcCourse f gcFinalQuadrant
    where
        f latA latB dAB = dacos $ (dsin latA - dsin latB * dcos dAB) / (dcos latB * dsin dAB)
