module GreatCircleTests ( gcTests ) where

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Ord (abs)
import Prelude (Unit, discard, negate, show, when, ($), (-), (/=), (<>), (>))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (fail, shouldEqual)

import LibNav.GreatCircle (gcDistance, gcFinalCourse, gcFinalQuadrant, gcInitCourse, gcInitQuadrant, getDir)
import LibNav.Types (Degrees(..), EW(..), NMiles, Posn(..), Quadrant(..), position)
import Test.Types (TestCase)

-- The math and worked examples for these tests taken from:
-- http://shipofficer.com/so/wp-content/uploads/2015/02/10.-Great-Circle-Sailing.pdf
-- A copy of this PDF is in the docs folder.

gcTests :: forall r. Spec r Unit
gcTests = describe "Great circle tests" do
    it "Calculates distance" do
        for_ gcTestData mkDistTest
    it "Calculates initial course" do
        for_ gcTestData mkInitCourseTest
    it "Calculates initial quadrant" do
        for_ gcTestData mkInitQuadTest
    it "Calculates EW" do
        for_ gcTestData mkEWTest
    it "Calculates final quadrant" do
        for_ finalTestData mkFinalQuadTest
    it "Calculates final course" do
        for_ finalTestData mkFinalCourseTest
  where
    finalTestData = filter (\(TestData td) -> td.hdg2 /= (Deg 0.0)) gcTestData

data TestData = TestData
    { title :: String
    , pos1 :: Posn
    , pos2 :: Posn
    , dist :: NMiles
    , hdg1 :: Degrees
    , quad1:: Quadrant
    , hdg2 :: Degrees
    , quad2:: Quadrant
    , ew   :: EW
    }

gcTestData :: Array TestData
gcTestData =
    [ TestData  { title: "Test Case 1"
                , pos1: position 56 20.0 ( -8) 12.0
                , pos2: position 52 12.0 (-57) 10.0
                , dist: 1696.5
                , hdg1: Deg 282.5766139
                , quad1: NW
                , hdg2: Deg 241.9789078
                , quad2: SW
                , ew: W
                }
    , TestData  { title: "Test Case 2"
                , pos1: position (-33) 22.0 113  8.0
                , pos2: position (-10) 51.0  49 16.0
                , dist: 3738.1
                , hdg1: Deg 275.2301109
                , quad1: SW
                , hdg2: Deg 302.1315333
                , quad2: NW
                , ew: W
                }
    , TestData  { title: "Test Case 3"
                , pos1: position 49 12.0 (-122) 50.0
                , pos2: position 13 30.0   145  15.0
                , dist: 4863.4
                , hdg1: Deg 280.3311666
                , quad1: NW
                , hdg2: Deg 0.0
                , quad2: NE 
                , ew: W
                }     -- Crosses meridian
    , TestData  { title: "Test Case 4"
                , pos1: position (-46) 20.0   169  10.0
                , pos2: position (-26) 25.0 (-105) 15.0
                , dist: 4099.1
                , hdg1: Deg 106.0742443
                , quad1: SE
                , hdg2: Deg 0.0
                , quad2: NE 
                , ew: E
                }     -- Crosses meridian
    , TestData  { title: "Test Case 5"
                , pos1: position (-17) 0.0   170  0.0
                , pos2: position   22  0.0 (-110) 0.0
                , dist: 5247.2
                , hdg1: Deg  66.0642527
                , quad1: SE
                , hdg2: Deg 70.5113954
                , quad2: NE 
                , ew: E
                }     -- Crosses meridian and equator
    ]

mkDistTest :: forall r. TestData -> TestCase r
mkDistTest (TestData td) = td.dist `shouldBeClose 0.05` gcDistance td.pos1 td.pos2

mkInitCourseTest :: forall r. TestData -> TestCase r
mkInitCourseTest (TestData td) = h1 `shouldBeClose 0.1` h2
  where
    (Deg h1) = td.hdg1
    (Deg h2) = gcInitCourse td.pos1 td.pos2

mkInitQuadTest :: forall r. TestData -> TestCase r
mkInitQuadTest (TestData td) = gcInitQuadrant td.pos1 td.pos2 `shouldEqual` td.quad1 

mkEWTest :: forall r. TestData -> TestCase r
mkEWTest (TestData td) = td.ew `shouldEqual` getDir p1.lon p2.lon
  where
    (Posn p1) = td.pos1
    (Posn p2) = td.pos2

mkFinalQuadTest :: forall r. TestData -> TestCase r
mkFinalQuadTest (TestData td) = td.quad2 `shouldEqual` gcFinalQuadrant td.pos1 td.pos2

mkFinalCourseTest :: forall r. TestData -> TestCase r
mkFinalCourseTest (TestData td) = h1 `shouldBeClose 0.1` h2
  where
    (Deg h1) = td.hdg2
    (Deg h2) = gcFinalCourse td.pos1 td.pos2

shouldBeClose :: forall r. Number -> Number -> Number -> TestCase r
shouldBeClose delta v1 v2 =
  when (abs (v1 - v2) > delta) $
    fail $ show v1 <> " ≠ " <> show v2