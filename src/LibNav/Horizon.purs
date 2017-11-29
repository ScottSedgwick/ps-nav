module LibNav.Horizon where

-- The math and worked examples for this unit taken from:
-- http://shipofficer.com/so/wp-content/uploads/2015/02/03.-Distance-of-Horizon.pdf
-- A copy of this PDF is in the docs folder.

import Prelude ((*))
import Math (sqrt)

import LibNav.Types

visibleHorizon :: Metres -> NMiles
visibleHorizon m = 2.07 * sqrt m

radarHorizon :: Metres -> NMiles
radarHorizon m = 2.21 * sqrt m