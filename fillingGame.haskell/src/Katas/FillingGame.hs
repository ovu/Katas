module Katas.FillingGame where

import Test.Hspec
import Data.List
import Data.Maybe

data Color = Red | Green | Blue deriving ( Eq, Show, Ord )

type Cell  = ( Int, Int, Color )

type Board = [Cell]

cellPositionX :: Cell -> Int
cellPositionX ( x, _, _ ) = x

cellPositionY :: Cell -> Int
cellPositionY ( _, y, _ ) = y

cellColor :: Cell -> Color
cellColor ( _, _, c ) = c

-----------------------------------------------------------------------------
-- Function neighborsBoundToOrigin
-- Input: a Cell and a list of Cells already bound to Origin
-- Output: the list of neighbors Cells, which are bound to the origin.
-- Description: Gets the list of Cells bound to the origin for a Cell given a list of Cells already bound.
neighborsBoundToOrigin :: Cell -> [Cell] -> [Cell]

neighborsBoundToOrigin point list = [ p | p <- list, cellColor p == cellColor point,
                                      (  cellPositionX p == cellPositionX point && cellPositionY p + 1 == cellPositionY point ) ||
                                      (  cellPositionY p == cellPositionY point && cellPositionX p + 1 == cellPositionX point ) ]

-- Specification of the function neighborsBoundToOrigin
neighborsBoundToOriginSpec :: Spec

neighborsBoundToOriginSpec = do
  describe "neighborsBoundToOriginSpec" $ do
    it "should return the list of neighbors on the X axis" $ do
      neighborsBoundToOrigin ( 1, 2, Red) [ ( 1, 1, Red )] `shouldBe` [ ( 1, 1, Red )]
    it "should return the list of neighbors on the Y axis" $ do
      neighborsBoundToOrigin ( 2, 1, Red) [ ( 1, 1, Red )] `shouldBe` [( 1, 1, Red )] 
    it "should not match neighbors on the diagonal" $ do
      neighborsBoundToOrigin ( 2, 2, Red) [ ( 1, 1, Red )] `shouldBe` []
    it "should not match neighbors with different colors" $ do
      neighborsBoundToOrigin ( 1, 2, Green) [ ( 1, 1, Red )] `shouldBe` []
    it "should not match neighbors with the same color bound diagonally to the origin" $ do
      neighborsBoundToOrigin ( 3, 3, Red) [ ( 1, 1, Red ), (1, 2, Red), (2, 2, Red)] `shouldBe` []


-- Function hasNeighborBoundToOrigin
-- Input: a Cell and a list of cells already bound to the origin 
-- Output: True when the Cell is bound to one of the cells in the list. Otherwise False. 
-- Description: Proof in a Cells is bound to origin, given the list of cells already bound to origin.
hasNeighborBoundToOrigin :: Cell -> [Cell] -> Bool

hasNeighborBoundToOrigin ( 1, 1, _ ) [_] = True
hasNeighborBoundToOrigin cell list = not  ( null ( neighborsBoundToOrigin cell list  ) ) 

-- Specification of the function neighborsBoundToOrigin
hasNeighborBoundToOriginSpec :: Spec
hasNeighborBoundToOriginSpec = do
  describe "hasNeighborBoundToOriginSpec" $ do
    it "should return true when the cell is 1,1" $ do
      hasNeighborBoundToOrigin ( 1, 1, Red ) [ ( 1, 1, Red ) ]  `shouldBe` True 
    it "should return true when a cell has a neighbor in the list of connected cells" $ do
      hasNeighborBoundToOrigin ( 1, 2, Red ) [ ( 1, 1, Red ) ]  `shouldBe` True 
    it "should return false when a cell does not have a neighbor in the list of connected cells" $ do
      hasNeighborBoundToOrigin ( 3, 3, Red ) [ ( 1, 1, Red ) ]  `shouldBe` False
    it "should return false when a cell has a neighbor diagonally connected to the Origin" $ do
      hasNeighborBoundToOrigin ( 3, 3, Red ) [ ( 1, 1, Red ), (1, 2, Red), (2, 2, Red) ]  `shouldBe` False

-- Function getCellsBoundToOriginGivenBoundCells 
-- Input: A list of Cells not bound to origin and a lis of Cells already bound to origin 
-- Output: The new list of Cells bound to the origin 
-- Description: Verifies if every Cell in the first list is bound to the origin and returns the new list of Cells bound to origin.
--              The function accumulates the new Cells that are bound to the origin and returns the new list as a result.
getCellsBoundToOriginGivenBoundCells :: [Cell] -> [Cell] -> [Cell]

getCellsBoundToOriginGivenBoundCells (x:xs) [] =  getCellsBoundToOriginGivenBoundCells xs [ x ]

getCellsBoundToOriginGivenBoundCells [] boundedCells = boundedCells 

getCellsBoundToOriginGivenBoundCells [p] boundedCells =  if hasNeighborBoundToOrigin p boundedCells then
                                                           p : boundedCells 
                                                         else
                                                            boundedCells 

getCellsBoundToOriginGivenBoundCells (x:xs) boundedCells = if hasNeighborBoundToOrigin x boundedCells then
                                                              getCellsBoundToOriginGivenBoundCells xs ( x:boundedCells )
                                                           else
                                                            getCellsBoundToOriginGivenBoundCells xs boundedCells

-- Specification of the function getCellsBoundToOriginGivenBoundCells
getCellsBoundToOriginGivenBoundCellsSpec :: Spec
getCellsBoundToOriginGivenBoundCellsSpec = do
  describe "getCellsBoundToOriginGivenBoundCellsSpec" $ do
    it "should return always the point 1,1 even when there is any other point with the same color" $ do
      getCellsBoundToOriginGivenBoundCells [( 1, 1, Red ), ( 1, 2, Blue )] [] `shouldBe` [( 1, 1, Red )]
    it "should return the list of points with the same color to the origin" $ do
      getCellsBoundToOriginGivenBoundCells [( 1, 2, Red )] [( 1, 1, Red )] `shouldBe` [( 1, 2, Red ), ( 1, 1, Red )]
    it "should return the connectedCells when the list of cells is empty" $ do
      getCellsBoundToOriginGivenBoundCells [] [( 1, 1, Red )] `shouldBe` [( 1, 1, Red )]
    it "should not return the cells connected to origin diagonally" $ do
      sort ( getCellsBoundToOriginGivenBoundCells [( 1, 1, Red ), (1, 2, Red), (2, 2, Red), (3, 3, Red), (4, 4, Red)] [] ) `shouldBe`
                                                                                                                            [( 1, 1, Red ), (1, 2, Red), (2, 2, Red)]

-- Function getColorOfOrigin
-- Input: the board
-- Output: the color of the origin. Cell in position (1, 1)
-- Description: Gets the color of the Cell in position (1, 1)
getColorOfOrigin :: Board -> Color

getColorOfOrigin board = cellColor ( head [c | c <- board, cellPositionX c == 1, cellPositionY c ==1 ] )

-- Function changeCellsFromColorToColor
-- Input: A list of Cells, the color that will be replaced by another color
-- Output: A list of cells where the Cells that matched with the first color were changed to the new color
-- Description: It searches for all the Cells that have the first color and replaces in all the Cells the new color.
changeCellsFromColorToColor :: [Cell] -> Color -> Color -> [Cell]

changeCellsFromColorToColor list color newColor = map (\ (a, b, c) -> if c == color then  (a, b, newColor) else (a, b, c)) list

-- Specification of the function changeCellsFromColorToColor
changeCellsFromColorToColorSpec :: Spec
changeCellsFromColorToColorSpec = do
  describe "changeCellsFromColorToColorSpec" $ do
    it "should return empty when the list is empty" $ do
      changeCellsFromColorToColor [] Red Green `shouldBe` [ ]
    it "should change the color of one Cell when the cell is equal the Color to be changed" $ do
      changeCellsFromColorToColor [(1, 1, Red)] Red Green `shouldBe` [ (1, 1, Green) ]
    it "should not change the color the Cell when the cell is not equal to the Color to be changed" $ do
      changeCellsFromColorToColor [(1, 1, Blue)] Red Green `shouldBe` [ (1, 1, Blue) ]
    it "should change just the cells that match the color to be changed" $ do
      changeCellsFromColorToColor [(1, 1, Blue), (1, 2, Red)] Red Green `shouldBe` [ (1, 1, Blue), (1, 2, Green) ]

-- Function getListOfCellsBoundToOriginWhenChangingToColor
-- Input: a list of Cells bound to origin. A list of Cells not bound to origin. The color the will get the Cells bound to origin.
-- Output: the list of bound Cells after the new Color was applied to both lists.
-- Description: Changes the color of the two lists (bound and not bound to the origin) and return the new list of Cells bound to origin.
getListOfCellsBoundToOriginWhenChangingToColor :: [Cell] -> [Cell] -> Color -> [Cell]

getListOfCellsBoundToOriginWhenChangingToColor boundToOrigin notBoundToOrigin color = getCellsBoundToOriginGivenBoundCells unboundCellsWithNewColor boundToOrigin
                                                where
                                                  unboundCellsWithNewColor  = changeCellsFromColorToColor notBoundToOrigin (getColorOfOrigin boundToOrigin) color

-- Function getColorsDifferentToColor 
-- Input: the color of the origin 
-- Output: the colors that are different to the origin
getColorsDifferentToColor :: Color -> [Color]
getColorsDifferentToColor Red   = [Green, Blue]
getColorsDifferentToColor Green = [Red, Blue]
getColorsDifferentToColor Blue  = [Red, Green]

-- Function selectNextColor 
-- Input: the board containing the colored Cells.
-- Output: the next color that will be used to color the Cells bound to the origin.
--         Returns Nothing when all the Cells in the Board have one Color.
-- Description: It changes the board to the two colors different from origin.
--              and returns the color that get more colored Cells bound to the origin.
selectNextColor :: Board -> Maybe Color
selectNextColor board = if  null cellsNotBoundToOrigin then Nothing
                        else
                          Just (if length (cellsBoundToOriginAfterChangingToColor firstColor)  >=
                                   length (cellsBoundToOriginAfterChangingToColor secondColor)
                                then firstColor
                                else secondColor)
                       where
                       colorOfOrigin = getColorOfOrigin board
                       colorsDifferentToOrigin = getColorsDifferentToColor colorOfOrigin
                       firstColor = head colorsDifferentToOrigin
                       secondColor = last colorsDifferentToOrigin
                       cellsBoundToOrigin = getCellsBoundToOriginGivenBoundCells board [ ]
                       cellsNotBoundToOrigin = [p | p <- board, p `notElem` cellsBoundToOrigin ]
                       unboundCellsWithNewColor = changeCellsFromColorToColor cellsNotBoundToOrigin colorOfOrigin
                       boundCellsWithNewColor = changeCellsFromColorToColor cellsBoundToOrigin colorOfOrigin
                       cellsBoundToOriginAfterChangingToColor color = getCellsBoundToOriginGivenBoundCells ( unboundCellsWithNewColor color ) ( boundCellsWithNewColor color )
                                           
-- Specification of the function selectNextColorSpec
selectNextColorSpec :: Spec
selectNextColorSpec = do
  describe "selectNextColorSpec" $ do
    it "should return Nothing when all cells are already connected to Origin with the same color" $ do
      selectNextColor [ (1, 1, Red), (1, 2, Red), (1, 3, Red)] `shouldBe` Nothing
    it "should return the color different to Origin that will color more cells" $ do
      selectNextColor [ (1, 1, Red), (1, 2, Red), (1, 3, Blue)] `shouldBe` Just Blue
    it "should return the color different to Origin when the other colors exist in the same number" $ do
      selectNextColor [ (1, 1, Red), (1, 2, Red), (1, 3, Green), (1, 4, Green), (2, 1, Blue), (3, 1, Blue)] `shouldBe` Just Green
    it "should return the color that will connect more colors to the origin without diagonals" $ do
      selectNextColor [ (1, 1, Red), (1, 2, Red), (2, 2, Blue), (3, 3, Blue), (4, 4 , Blue), (2, 1, Green), (3, 1, Green)] `shouldBe` Just Green

-- Function changeCellsBoundToOriginToNewColor
-- Input: The Board and the Color that will get the Cells bound to the origin 
-- Output: The Board where the Cells that are bound to origin have the new color. 
changeCellsBoundToOriginToNewColor :: Board -> Color -> Board

changeCellsBoundToOriginToNewColor board newColor = cellsBoundToOriginWithNewColor ++ cellsNotBoundToOrigin 
                                        where 
                                           cellsBoundToOrigin = getCellsBoundToOriginGivenBoundCells board [ ]
                                           colorOfOrigin = getColorOfOrigin board
                                           cellsBoundToOriginWithNewColor = changeCellsFromColorToColor cellsBoundToOrigin colorOfOrigin newColor
                                           cellsNotBoundToOrigin = [p | p <- board, p `notElem` cellsBoundToOrigin ]

-- Specification of the function changeCellsBoundToOriginToNewColor
changeCellsBoundToOriginToNewColorSpec :: Spec
changeCellsBoundToOriginToNewColorSpec = do
  describe "changeCellsBoundToOriginToNewColorSpec" $ do
    it "should change the color of all the cells bound to the origin" $ do
      sort ( changeCellsBoundToOriginToNewColor [(1, 1, Red), (1, 2, Red), (1, 3, Red), 
                                                 (2, 1, Blue), (2, 2, Red), (2, 3, Blue), 
                                                 (3, 1, Blue), (3, 2, Blue), (3, 3, Green), 
                                                 (4, 1, Blue), (4, 2, Green), (4, 3, Red), 
                                                 (5, 1, Blue), (5, 2, Green), (5, 3, Red), 
                                                 (6, 1, Red), (6, 2, Blue), (6, 3, Red)] Blue ) `shouldBe` 
                                                        [(1, 1, Blue), (1, 2, Blue), (1, 3, Blue), 
                                                         (2, 1, Blue), (2, 2, Blue), (2, 3, Blue), 
                                                         (3, 1, Blue), (3, 2, Blue), (3, 3, Green),
                                                         (4, 1, Blue), (4, 2, Green), (4, 3, Red),
                                                         (5, 1, Blue), (5, 2, Green), (5, 3, Red),
                                                         (6, 1, Red), (6, 2, Blue), (6, 3, Red)]

-- Function getColorsToGetTheBoardColored
-- Input: The Board containing colored Cells 
-- Output: the list of Colors to get the complete Board in one color.
-- Description: It is the main function. It return the colors, that should get the Cells bound to origin, to be applied step by step
--              to get the Board where every Cells have just one color. 
getColorsToGetTheBoardColored :: Board -> [Color]

getColorsToGetTheBoardColored board =  if isNothing nextColor then []
                                       else fromJust nextColor: getColorsToGetTheBoardColored ( sort cellsWithBoundCellsWithNewColor )
                                       where 
                                         nextColor = selectNextColor board
                                         cellsWithBoundCellsWithNewColor = changeCellsBoundToOriginToNewColor board ( fromJust nextColor )

-- This specification is used to test the algorithm. It applies the main function (getColorsToGetTheBoardColored) to a given Board
-- and compares the result with the expected list of Colors.
getColorsToGetTheBoardColoredSpec :: Spec
getColorsToGetTheBoardColoredSpec = do
  describe "getColorsToGetTheBoardColoredSpec" $ do
    it "should return the list of colors representing the steps to get the board with one color" $ do
      getColorsToGetTheBoardColored [(1, 1, Red), (1, 2, Green), (1, 3, Red), (1, 4, Blue), (1, 5, Red), (1, 6, Green),
                                     (2, 1, Blue), (2, 2, Red), (2, 3, Blue), (2, 4, Green), (2, 5, Blue), (2, 6, Green),
                                     (3, 1, Blue), (3, 2, Blue), (3, 3, Green), (3, 4, Blue), (3, 5, Blue), (3, 6, Blue),
                                     (4, 1, Blue), (4, 2, Green), (4, 3, Red), (4, 4, Blue), (4, 5, Red), (4, 6, Green),
                                     (5, 1, Blue), (5, 2, Green), (5, 3, Red), (5, 4, Green), (5, 5, Green), (5, 6, Green),
                                     (6, 1, Red), (6, 2, Blue), (6, 3, Red), (6, 4, Green), (6, 5, Blue), (6, 6, Red) ] `shouldBe`
                                      [Blue, Green, Red, Blue, Green, Red, Green, Blue]
