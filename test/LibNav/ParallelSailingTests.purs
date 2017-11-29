module ParallelSailingTests ( parallelSailingTests ) where

import Data.Foldable (for_)
import Prelude (negate)
import Test.Spec (describe, it)

import LibNav.ParallelSailing (departure)
import LibNav.Types (Posn(..), NMiles, position)
import Test.Types (TestCase, TestSuite, shouldNearlyBe)

-- Unit tests from http://shipofficer.com/so/wp-content/uploads/2015/02/5.-Parallel-Sailing.pdf

data TestData = TestData 
    { title :: String
    , pos1 :: Posn
    , pos2 :: Posn
    , dist :: NMiles
    }

testData :: Array TestData
testData = [ TestData { title: "Test Case 1", pos1: position 35 20.0 (-15) 31.0,   pos2: position 35 20.0 (-25) 50.0, dist: 505.0 }
           , TestData { title: "Test Case 2", pos1: position (-30) 0.0 (-171) 0.0, pos2: position (-30) 0.0 178 0.0,  dist: 571.6 }
           ]

mkTest :: forall r. TestData -> TestCase r
mkTest (TestData td) = departure p1.lat p1.lon p2.lon `shouldNearlyBe 0.03` td.dist
  where
    (Posn p1) = td.pos1
    (Posn p2) = td.pos2

parallelSailingTests :: forall r. TestSuite r
parallelSailingTests = describe "Parallel sailing tests" do
    it "Calculates departure" do
        for_ testData mkTest