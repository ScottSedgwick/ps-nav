module LibNav.ParallelSailing (departure) where

import LibNav.Types

import Math (abs, cos, pi)
import Prelude (($), (*), (-), (>))

departure :: Degrees -> Degrees -> Degrees -> NMiles
departure lat lon1 lon2 = d * 60.0 
    where 
        rlon1   = degToRad lon1
        rlon2   = degToRad lon2
        rlat    = degToRad lat
        dlon'   = abs $ rlon2 - rlon1
        dlon    = if dlon' > pi then (pi * 2.0) - dlon' else dlon'
        rd      = abs $ dlon * cos rlat
        (Deg d) = radToDeg rd