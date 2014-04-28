module Main where

import Katas.FillingGame
import Test.Hspec


main :: IO ()
main = hspec $ do
       hasNeighborBoundToOriginSpec
       neighborsBoundToOriginSpec
       getCellsBoundToOriginGivenBoundCellsSpec
       changeCellsFromColorToColorSpec
       selectNextColorSpec
       changeCellsBoundToOriginToNewColorSpec
       getColorsToGetTheBoardColoredSpec

