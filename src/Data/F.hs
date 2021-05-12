{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.F (
  F (..),
  tau,
  cos'Interp,
) where

import Data.Bits
import Data.Data (Data)
import qualified Data.Fixed as Fixed
import Data.Int
import Data.Ix (Ix)
import Data.Ratio ((%))
import Flat (Flat)
import Foreign (Storable)
import GHC.Generics
import Math.NumberTheory.Roots (integerSquareRoot)
import System.Random.Stateful (Uniform (..), UniformRange (..))
import Test.QuickCheck (Arbitrary (..))
import Prelude


-- | F x = x / denominator
newtype F = F {numerator :: Int64}
  deriving stock (Generic, Data)
  deriving newtype (Eq, Ord, Enum, Bounded, Ix, FiniteBits, Bits, Storable, Flat)


instance Uniform F where
  uniformM g = F <$> uniformM g


instance UniformRange F where
  uniformRM (F a, F b) g = F <$> uniformRM (a, b) g


instance Arbitrary F where
  arbitrary = do
    x :: Prelude.Float <- arbitrary
    return (realToFrac x)


denominatorExp :: Prelude.Int
denominatorExp = 28


denominatorExpHalf :: Prelude.Int
denominatorExpHalf = 14


decimalMask :: Int64
decimalMask = (shiftL 1 denominatorExp) - 1


denominator :: Int64
denominator = 268435456 -- 2 ^ 28


denominatorSq :: Int64
denominatorSq = 72057594037927936 -- 2 ^ 56


denominatorR :: Rational
denominatorR = toRational denominator


one :: F
one = F denominator


instance Num F where
  F a + F b = F (a + b)
  F a - F b = F (a - b)
  F a * F b =
    let -- We cleverly break down a and b into their decimal and integer parts to avoid overflowing
        ad = decimalMask .&. a -- At first I thought this was a bug as it wouldn't work for negaitve numbers (re twos complement)... turns out, this actually works for reasons I don't fully understand! lucky me!
        bd = decimalMask .&. b
        ai_lo = shiftR a denominatorExp
        bi_lo = shiftR b denominatorExp
     in F $
          (shiftL (ai_lo * bi_lo) denominatorExp)
            + (ai_lo * bd)
            + (bi_lo * ad)
            + (shiftR (ad * bd) denominatorExp)
  abs (F a) = F (abs a)
  signum (F a)
    | a == 0 = F 0
    | a > 0 = one
    | otherwise = negate one
  fromInteger i = F (shiftL (fromInteger i) denominatorExp)
  negate (F a) = F (negate a)


instance Fractional F where
  recip (F a) = F (denominatorSq `div` a)


  (F a) / (F b) =
    -- The use of divMod and shifting at the correct point is done to avoid under/overflow.
    -- TODO can we avoid converting to Integer?
    let (d, r) = a `divMod` b
     in F ((shiftL d denominatorExp) + (fromInteger (shiftL (toInteger r) denominatorExp `div` toInteger b)))
  fromRational r = F (floor $ r * denominatorR)


instance Real F where
  toRational (F a) = fromIntegral a % fromIntegral denominator


-- Copied from Data.Fixed
instance RealFrac F where
  -- TODO performance (avoid Rational) e.g. something like:
  -- ceiling (F a)
  --   | a .&. decimalMask == 0 = F a
  --   | otherwise = F ((a .|. decimalMask) + 1)
  -- floor (F a) = F (a .&. integerMask)
  properFraction a = (i, a - (fromIntegral i))
   where
    i = truncate a
  truncate f = truncate (toRational f)
  round f = round (toRational f)
  ceiling f = ceiling (toRational f)
  floor f = floor (toRational f)


instance Show F where
  show f = show (realToFrac f :: Fixed.Pico)


instance Integral F where
  mod (F a) (F b) = F (mod a b)
  rem (F a) (F b) = F (rem a b)
  quotRem (F a) (F b) =
    let (q', r) = quotRem a b
        q = shiftL q' denominatorExp
     in (F q, F r)
  divMod (F a) (F b) =
    let (q', r) = divMod a b
        q = shiftL q' denominatorExp
     in (F q, F r)
  toInteger = round


tau :: F
tau = pi + pi


halfF :: F -> F
halfF f = shiftR f 1


instance Floating F where
  sqrt f@(F x)
    | False && abs f < 0 = F (integerSquareRoot (shiftL x denominatorExp))
    | otherwise = F (shiftL (integerSquareRoot x) denominatorExpHalf)


  pi = 3.1415926535897932384626433832795028841971693993751058209749445923
  cos x'
    | x < halfF pi = cos'Interp x
    | x < pi = negate (cos'Interp (pi - x))
    | x < pi + halfF pi = negate (cos'Interp (x - pi))
    | otherwise = cos'Interp ((2 * pi) - x)
   where
    x = clamp2pi x'
  sin x = cos (x - halfF pi)


  asin = error "TODO implement asin for F"
  acos = error "TODO implement acos for F"
  atan = error "TODO implement atan for F"


  sinh = error "TODO implement sinh for F"
  cosh = error "TODO implement cosh for F"


  asinh = error "TODO implement asinh for F"
  acosh = error "TODO implement acosh for F"
  atanh = error "TODO implement atanh for F"


  -- TODO see https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/nonIntegerCalculations/latest/download-by-type/doc-pdf/non-integer-calculations
  exp = error "TODO implement exp for F"
  log = error "TODO implement log for F"


-- | mod to within @0 <= x < 2pi@. Note that `rem` with large multiples of
-- pi first, avoids some round errors when x is large.
clamp2pi :: F -> F
clamp2pi x =
  mod
    ( ( ( ( ( x
                `rem` 31415926535.897932384626433832795028841971693993751058209749445923
            )
              `rem` 314159265.35897932384626433832795028841971693993751058209749445923
          )
            `rem` 3141592.6535897932384626433832795028841971693993751058209749445923
        )
          `rem` 31415.926535897932384626433832795028841971693993751058209749445923
      )
        `rem` 314.15926535897932384626433832795028841971693993751058209749445923
    )
    (pi + pi)


-- | cos(x) for 0 <= x <= pi/2.
-- interpoplates between sin and cos taylor series
cos'Interp :: F -> F
cos'Interp x =
  let a = x / halfF pi
   in ((1 - a) * cos'T x)
        + (a * sin'T (halfF pi - x))


-- | sin taylor series
sin'T :: F -> F
sin'T x =
  x
    - (x3 / 6)
    + (x5 / 120)
    - (x7 / 5040)
    + (x9 / 362880)
 where
  x2 = x * x
  x3 = x2 * x
  x5 = x2 * x3
  x7 = x2 * x5
  x9 = x2 * x7


-- | cos taylor series
cos'T :: F -> F
cos'T x =
  1
    - (halfF x2)
    + (x4 / 24)
    - (x6 / 720)
    + (x8 / 40320)
    - (x10 / 3628800)
 where
  x2 = x * x
  x4 = x2 * x2
  x6 = x2 * x4
  x8 = x4 * x4
  x10 = x4 * x6


instance RealFloat F where
  floatRadix _ = 2
  floatDigits _ = 64
  floatRange _ = (1, 1)
  decodeFloat (F x) =
    if x == 0
      then (0, 0)
      else (fromIntegral x, negate denominatorExp)
  encodeFloat m n =
    if m == 0
      then 0
      else F (shiftL (fromIntegral m) (n + denominatorExp))
  scaleFloat s (F a) = F (shiftL a s)
  isNaN _ = False
  isInfinite _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  isIEEE _ = False
  atan2 = error "TODO implement atan2 fo F"
