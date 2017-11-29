module PlaneSailingTests where

import Data.Foldable (for_)
import Prelude (negate, discard)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

import LibNav.PlaneSailing (planeCourse, planeDistance, planeDR, planeQuadrant)
import LibNav.Types (Degrees(..), NMiles, Posn(..), Quadrant(..), position)
import Test.Types (TestCase, TestSuite, shouldNearlyBe)

data TestData = TestData
    { pos1 :: Posn
    , pos2 :: Posn
    , hdg  :: Degrees
    , dist :: NMiles
    , quad :: Quadrant
    }

testData :: Array TestData
testData = [ TestData { pos1: position 27 15.0 (-71) 23.0
                      , pos2: position 28 11.0 (-68) 18.0
                      , hdg: Deg 71.122537
                      , dist: 173.1
                      , quad: NE
                      }
           , TestData { pos1: position 35 15.0 (-62) 23.0
                      , pos2: position 30 25.0 (-70) 18.0
                      , hdg: Deg 233.997857
                      , dist: 493.4
                      , quad: SW
                      }
           , TestData { pos1: position (-40) 25.0 175 50.0
                      , pos2: position (-35) 3.61 (-176) 5.66
                      , hdg: Deg 50.0003565
                      , dist: 500.0
                      , quad: NE
                      }
           ]

mkQuadTest :: forall r. TestData -> TestCase r
mkQuadTest (TestData td) = planeQuadrant td.pos1 td.pos2 `shouldEqual` td.quad

mkCourseTest :: forall r. TestData -> TestCase r
mkCourseTest (TestData td) = actual `shouldNearlyBe 0.01` expected
    where
      (Deg actual)   = planeCourse td.pos1 td.pos2
      (Deg expected) = td.hdg

mkDistanceTest :: forall r. TestData -> TestCase r
mkDistanceTest (TestData td) = actual `shouldNearlyBe 0.06` expected
    where
      actual   = planeDistance td.pos1 td.pos2
      expected = td.dist

mkDRTest :: forall r. TestData -> TestCase r
mkDRTest (TestData td) = do
    actualLat `shouldNearlyBe 0.03` expectLat
    actualLon `shouldNearlyBe 0.03` expectLon
    where
      (Posn {lat: (Deg actualLat), lon: (Deg actualLon)}) = planeDR td.pos1 td.hdg td.dist
      (Posn {lat: (Deg expectLat), lon: (Deg expectLon)}) = td.pos2
 
planeSailingTests :: forall r. TestSuite r
planeSailingTests = describe "Plane sailing tests" do
    it "Calculates plane course" do
        for_ testData mkCourseTest
    it "Calculates plane distance" do
        for_ testData mkDistanceTest
    it "Calculates plane quad" do
        for_ testData mkQuadTest
    it "Calculates dead reckoning position" do
        for_ testData mkDRTest