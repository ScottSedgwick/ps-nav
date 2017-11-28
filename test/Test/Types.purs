module Test.Types (TestCase, TestSuite, shouldNearlyBe) where
  
import Control.Monad.Aff (Aff)
import Prelude (Unit, ($), when, (>), (-), (<>), show)
import Math (abs)
import Test.Spec (Spec)
import Test.Spec.Assertions (fail)

type TestCase r = Aff r Unit
type TestSuite r = Spec r Unit

shouldNearlyBe :: forall r. Number -> Number -> Number -> TestCase r
shouldNearlyBe delta v1 v2 =
  when (abs (v1 - v2) > delta) $
    fail $ show v1 <> " ≠ " <> show v2