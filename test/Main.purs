module Test.Main where

import Prelude (Unit, discard)

import Control.Monad.Eff (Eff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import GreatCircleTests (gcTests)
import HorizonTests (horizonTests)
import ParallelSailingTests (parallelSailingTests)
import PlaneSailingTests (planeSailingTests)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
    -- planeSailingTests
    gcTests
    horizonTests
    parallelSailingTests
    planeSailingTests
  
