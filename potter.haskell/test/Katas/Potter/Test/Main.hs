module Main where

import Katas.Potter
import Test.Hspec


main :: IO ()
main = hspec $ do
  decreaseNumberOfBooksSpec
  makeGroupOfBooksSpec 
  calculatePriceForGroupOfBooksSpec
  calculatePriceForBooksSpec
