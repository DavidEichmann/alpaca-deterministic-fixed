{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.F
import Data.Int
import Test.Tasty
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
    , testProperty
        "sin"
        (\(x :: F) -> vsDouble 0.01 sin x)
    , testProperty
        "cos"
        (\(x :: F) -> vsDouble 0.01 cos x)
    , testProperty
        "tan"
        (\(x :: F) -> vsDouble 0.01 tan x)
    ]
