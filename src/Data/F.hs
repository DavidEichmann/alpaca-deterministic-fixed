{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.F (
  F (..),
  tau,
) where

import Data.Bits
import Data.Data (Data)
import qualified Data.Fixed as Fixed
import Data.Int
import Data.Ix (Ix)
import Data.Ratio ((%))
import Foreign (Storable)
import GHC.Generics
import Math.NumberTheory.Roots (integerSquareRoot)
import Prelude


-- | F x = x / denominator
newtype F = F {numerator :: Int64}
  deriving (Generic, Eq, Ord, Enum, Bounded, Data, Ix, FiniteBits, Bits, Storable)


denominatorExp :: Prelude.Int
denominatorExp = 28


denominatorExpHalf :: Prelude.Int
denominatorExpHalf = 14


decimalMask :: Int64
decimalMask = (shiftL 1 denominatorExp) - 1


integerMask :: Int64
integerMask = complement decimalMask


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
  (F a) / (F b) = F ((shiftL a denominatorExp) `div` b) --  TODO you could also break this into integer/decimal parts in order to avoid the overflow from shiftL
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
  show f = show (realToFrac f :: Fixed.Pico) -- TODO How many decimal places do we actually need?


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


halff :: F -> F
halff f = shiftR f 1


instance Floating F where
  sqrt f@(F x)
    | False && abs f < 0 = F (integerSquareRoot (shiftL x denominatorExp))
    | otherwise = F (shiftL (integerSquareRoot x) denominatorExpHalf)


  pi = 3.1415926535897932384626433832795028841971693993751058209749445923


  -- Taylor series
  sin x' = sin' (mod x' (pi + pi))
   where
    -- 0 <= x <= 2 pi
    sin' x
      | x < pi / 2 = sin'' x
      | x < pi = sin'' (pi - x)
      | x < (pi + pi + pi) / 2 = negate (sin'' (x - pi))
      | otherwise = negate (sin'' (pi + pi - x))

    -- -pi / 2 <= x <= -pi / 2
    -- TODO let's interpolate between taylor expansion at 0 and at pi/2
    sin'' x =
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


  exp = error "TODO implement exp for F"
  log = error "TODO implement log for F"
  cos x = sin (x + (halff pi))
  asin = error "TODO implement asin for F"
  acos = error "TODO implement acos for F"
  atan = error "TODO implement atan for F"
  sinh = error "TODO implement sinh for F"
  cosh = error "TODO implement cosh for F"
  asinh = error "TODO implement asinh for F"
  acosh = error "TODO implement acosh for F"
  atanh = error "TODO implement atanh for F"


-- TODO RealFloat
instance RealFloat F where
  floatRadix = error "TODO implement floatRadix for F"
  floatDigits = error "TODO implement floatDigits for F"
  floatRange = error "TODO implement floatRange for F"
  decodeFloat = error "TODO implement decodeFloat for F"
  encodeFloat = error "TODO implement encodeFloat for F"
  isNaN = error "TODO implement isNaN for F"
  isInfinite = error "TODO implement isInfinite for F"
  isDenormalized = error "TODO implement isDenormalized for F"
  isNegativeZero = error "TODO implement isNegativeZero for F"
  isIEEE = error "TODO implement isIEEE for F"
