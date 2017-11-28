module LibNav.Types where

import Prelude (class CommutativeRing, class Eq, class Ord, class Ring, class Semiring, class Show, negate, show, ($), (&&), (*), (+), (-), (/), (<>), (==))

import Data.DivisionRing (class DivisionRing)
import Data.EuclideanRing (class EuclideanRing, mod, div)
import Data.Fractional (class Fractional, fromRational)
import Data.Int (toNumber, round)
import Data.Num (class Num, abs, fromBigInt, signum)
import Data.Ord (compare)
import Data.Real (class Real, toRational)
import Data.Tuple (Tuple(..))
import Math (Radians, pi, trunc, exp, log, sin, cos, asin, acos, atan) --, sinh, cosh, asinh, acosh, atanh)

mantissa :: Number -> Number
mantissa x = x - trunc x

tau :: Number
tau = pi * 2.0

d2r :: Number -> Number
d2r d = tau * d / 360.0

degToRad :: Degrees -> Radians
degToRad (Deg d) = d2r d

r2d :: Number -> Number
r2d r = r * 360.0 / tau

radToDeg :: Radians -> Degrees
radToDeg r = Deg (r2d r)

newtype Degrees = Deg Number 
instance degreesOrd :: Ord Degrees where
  compare (Deg a) (Deg b) = compare a b
instance degreesShow :: Show Degrees where
  show (Deg a) = "Deg " <> show a
derive instance degreesEq :: Eq Degrees 
instance degreesSemiring :: Semiring Degrees where
  one = Deg 1.0
  mul (Deg a) (Deg b) = Deg (a * b)
  zero = Deg 0.0
  add (Deg a) (Deg b) = Deg (a + b)
instance degreesRing :: Ring Degrees where
  sub (Deg a) (Deg b) = Deg (a - b)
instance degreesCummutativeRing :: CommutativeRing Degrees where
instance degreesNum :: Num Degrees where
  fromBigInt a = Deg $ fromBigInt a
  signum (Deg a) = Deg $ signum a
  abs (Deg a) = Deg $ abs a
  negate (Deg a) = Deg $ negate a
instance degreesReal :: Real Degrees where
  toRational (Deg x) = toRational x
instance degreesDivisionRing :: DivisionRing Degrees where
  recip (Deg a) = Deg (1.0 / a)
instance degreesFractional :: Fractional Degrees where
  fromRational x = Deg $ fromRational x
instance degreesEuclideanRing :: EuclideanRing Degrees where
  mod (Deg a) (Deg b) = Deg (a `mod` b)
  div (Deg a) (Deg b) = Deg (a `div` b)
  degree (Deg a) = 0

dpi :: Degrees
dpi = Deg pi

dexp :: Degrees -> Degrees
dexp (Deg x) = Deg $ exp x

dlog :: Degrees -> Degrees
dlog (Deg x) = Deg $ log x

dsin :: Degrees -> Degrees
dsin (Deg x) = Deg $ sin $ d2r x

dcos :: Degrees -> Degrees
dcos (Deg x) = Deg $ cos $ d2r x

dasin :: Degrees -> Degrees
dasin (Deg x) = Deg $ r2d $ asin x

dacos :: Degrees -> Degrees
dacos (Deg x) = Deg $ r2d $ acos x

datan :: Degrees -> Degrees
datan (Deg x) = Deg $ r2d $ atan x
-- dsinh (Deg x) = Deg $ sinh $ d2r x
-- dcosh (Deg x) = Deg $ cosh $ d2r x
-- dasinh (Deg x) = Deg $ r2d $ asinh x
-- dacosh (Deg x) = Deg $ r2d $ acosh x
-- datanh (Deg x) = Deg $ r2d $ atanh x

type Knots = Number
type Lat = Degrees
type Lon = Degrees
type Hours = Number
type NMiles = Number
type Metres = Number
data Quadrant = NE | SE | NW | SW 
derive instance quadrantEq :: Eq Quadrant
instance quadrantShow :: Show Quadrant where
  show NE = "NE"
  show SE = "NE"
  show NW = "NW"
  show SW = "SW"
data EW = E | W 
derive instance ewEq :: Eq EW
instance ewShow :: Show EW where
  show E = "E"
  show W = "W"

type DegMin = Tuple Int Number

dmToDeg :: DegMin -> Degrees
dmToDeg (Tuple d m) = Deg (toNumber d + m / 60.0)

degToDm :: Degrees -> DegMin
degToDm (Deg ds) = (Tuple (round d) m)
  where
    d = trunc ds
    m = (ds - d) * 60.0

newtype Posn = Posn { lat :: Lat, lon :: Lon } 
instance posnShow :: Show Posn where
  show (Posn {lat: a, lon: b}) = "Posn Lat: " <> show a <> ", Lon: " <> show b
instance posnEq :: Eq Posn where
  eq (Posn p1) (Posn p2) = (p1.lat == p2.lat) && (p1.lon == p2.lon)

position :: Int -> Number -> Int -> Number -> Posn
position latDeg latMin lonDeg lonMin = Posn { lat: Deg latitude, lon: Deg longtitude }
    where
        latitude   = toNumber latDeg + toNumber (signum latDeg) * (latMin / 60.0)
        longtitude = toNumber lonDeg + toNumber (signum lonDeg) * (lonMin / 60.0)
