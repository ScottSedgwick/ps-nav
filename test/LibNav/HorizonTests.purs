module HorizonTests ( horizonTests ) where

import Prelude (discard)
import Test.Spec (describe, it)

import LibNav.Horizon
import Test.Types (TestSuite, shouldNearlyBe)

-- The math and worked examples for these tests taken from:
-- http://shipofficer.com/so/wp-content/uploads/2015/02/3.-Distance-of-Horizon.pdf
-- A copy of this PDF is in the docs folder.

horizonTests :: forall r. TestSuite r
horizonTests = describe "Horizon tests" do
    it "should calculate visible horizon" do
        (visibleHorizon 15.0) `shouldNearlyBe 0.01` 8.02 
    it "should calculate radar horizon" do
        (radarHorizon 23.0) `shouldNearlyBe 0.01` 10.6 
