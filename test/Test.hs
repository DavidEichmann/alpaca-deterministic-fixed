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
approxEq tolerance a b = abs (a - b) < tolerance


-- | Check that a funciton gives similar results for F and for Double
vsDouble :: F -> (forall a. Floating a => a -> a) -> F -> Bool
vsDouble tolerance fn f =
  approxEq
    (realToFrac tolerance)
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
        (\(x :: F) -> vsDouble 0.0001 sin x)
    , testProperty
        "cos"
        (\(x :: F) -> vsDouble 0.0001 cos x)
    ]
