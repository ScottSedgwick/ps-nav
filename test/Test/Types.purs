module Test.Types (TestCase) where
  
import Control.Monad.Aff (Aff)
import Prelude (Unit)

type TestCase r = Aff r Unit