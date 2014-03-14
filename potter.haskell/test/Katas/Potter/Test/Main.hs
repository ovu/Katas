module Main where

import Katas.Potter
import Test.Hspec


main :: IO ()
main = hspec $ do
  countNonZeroBooksSpec
  decreaseNonZeroBooksSpec
  makeGroupsSpec 
  calculatePriceForGroupSpec
  calculatePriceSpec 
