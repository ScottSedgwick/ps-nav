module Test.Main where

import Prelude (Unit, discard, pure, unit, ($))

import Control.Monad.Eff (Eff)
-- import Data.Num as N
-- import Data.Foldable (for_)
-- import Math (abs, cos, atan)
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import GreatCircleTests

-- newtype TestData = TestData { pos1 :: Posn, pos2 :: Posn, hdg :: Degrees, dist :: NMiles, quad :: Quadrant}

-- errDist :: NMiles
-- errDist = 0.05

-- testData :: Array TestData
-- testData = [ TestData {pos1: position 27 15.0 (-71) 23.0,  pos2: position 28 11.0 (-68) 18.0,  hdg: Deg  71.122537, dist: 173.1, quad: NE}
--            , TestData {pos1: position 35 15.0 (-62) 23.0,  pos2: position 30 25.0 (-70) 18.0,  hdg: Deg 233.997857, dist: 493.4, quad: SW}
--            , TestData {pos1: position (-40) 25.0 175 50.0, pos2: position (-35) 3.61 (-176) 5.66, hdg: Deg 50.0003565, dist: 500.0, quad: NE}
--            ]

-- mkQuadTest :: TestData -> Test
-- mkQuadTest (p1, p2, c, d, q) = q ~=? planeQuadrant p1 p2

-- mkCourseTest :: forall r. TestData -> Spec r Unit
-- mkCourseTest (TestData td) = 
--   it "Should calculate the correct DR course" do
--     td.hdg `shouldEqual` planeCourse td.pos1 td.pos2

-- mkDistanceTest :: forall r. TestData -> Spec r Unit
-- mkDistanceTest (TestData td) = 
--   it "Should calculate the correct DR distance" do
--     td.dist `shouldEqual` planeDistance td.pos1 td.pos2
--TestCase $ assertEquals "Distance Incorrect" errDist d (planeDistance p1 p2)

-- mkDRTest :: TestData -> Test
-- mkDRTest (p1, p2, c, d, q) = TestCase $ assertEqual "DR Incorrect" p2 (planeDR p1 c d)

-- planeSailingTests :: forall r. Spec r Unit
-- planeSailingTests = 
--   describe "Course tests" do
--     for_ testData mkCourseTest      
    -- describe "Course tests" do
    --   it "Should calculate the correct dead reckoning course" do
    --     let td = {pos1: position 27.0 15.0 (-71.0) 23.0,  pos2: position 28.0 11.0 (-68.0) 18.0,  hdg: Deg  71.122537, dist: 173.1, quad: NE}
    --     let hdg = planeCourse td.pos1 td.pos2
    --     td.hdg `shouldEqual` hdg
--     where
--         courseTests = map mkCourseTest testData
--         distanceTests = map mkDistanceTest testData
--         quadrantTests = map mkQuadTest testData
--         drTests = map mkDRTest testData

demoSpec :: forall r. Spec r Unit
demoSpec = 
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
      pending "feature complete"
    describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports streaming reporters" $ pure unit
      it "is PureScript 0.10.x compatible" $ pure unit

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
    -- planeSailingTests
    demoSpec
    gcTests
  
