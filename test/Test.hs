{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM_)
import Data.F
import Data.Int
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain tests


instance Arbitrary F where
  arbitrary = F <$> arbitrary


infix 4 ~=


(~=) :: (Num a, Ord a, Fractional a) => a -> a -> Bool
(~=) = approxEq 0.00001


approxEq :: (Num a, Ord a, Fractional a) => a -> a -> a -> Bool
approxEq relativeTolerance actual expected = abs (actual - expected) < max 0.000001 (relativeTolerance * abs expected)


-- | Check that a funciton gives similar results for F and for Double
vsDouble :: F -> (forall a. Floating a => a -> a) -> F -> Bool
vsDouble relativeTolerance fn f =
  approxEq
    (realToFrac relativeTolerance)
    (realToFrac (fn f))
    (fn (realToFrac f :: Double))


-- | Check the error compared to Double
vsDoubleError :: (forall a. Floating a => a -> a) -> F -> Double
vsDoubleError fn f =
  abs $
    (realToFrac (fn f))
      - (fn (realToFrac f :: Double))


tests :: TestTree
tests =
  testGroup
    "F"
    [ testProperty
        "quot rem"
        ( \(x :: F) y ->
            y /= 0
              ==> (x == y * quot x y + rem x y)
              && ((rem x y == 0) || (abs (rem x y) < abs y))
        )
    , testProperty
        "div mod"
        ( \(x :: F) y ->
            y /= 0
              ==> (x == y * div x y + mod x y)
              && ((mod x y == 0) || (abs (mod x y) < abs y))
        )
    , testCase
        "sin (0..2pi)"
        ( forM_ [0, 0.01 .. 2 * pi] $
            \x ->
              (vsDoubleError sin x) < 0.000001
                @? ("sin " ++ show x ++ " = " ++ show (sin x) ++ " (expected " ++ show (sin (realToFrac x :: Double)) ++ ")")
        )
    , testCase
        "cos (0..2pi)"
        ( forM_ [0, 0.01 .. 2 * pi] $
            \x ->
              (vsDoubleError cos x) < 0.000001
                @? ("cos " ++ show x ++ " = " ++ show (cos x) ++ " (expected " ++ show (cos (realToFrac x :: Double)) ++ ")")
        )
    , testCase
        "tan (0..pi/2)"
        ( forM_ [0, 0.01 .. (pi / 2) - 0.01] $
            \x ->
              (vsDoubleError cos x) < 0.000001
                @? ("tan " ++ show x ++ " = " ++ show (tan x) ++ " (expected " ++ show (tan (realToFrac x :: Double)) ++ ")")
        )
    , testProperty
        "sin"
        (\(x :: F) -> vsDouble 0.0001 sin x)
    , testProperty
        "cos"
        (\(x :: F) -> vsDouble 0.0001 cos x)
    , testProperty
        "tan"
        (\(x :: F) -> vsDouble 0.001 tan x)
    ]
