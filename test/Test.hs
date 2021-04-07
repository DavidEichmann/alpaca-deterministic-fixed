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


(~=) :: F -> F -> Bool
a ~= b = abs (a - b) < 0.00001


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
        "sin^2 + cos^2 ~= 1"
        (\(x :: F) -> (sin x ^ 2) + (cos x ^ 2) ~= 1)
    ]
