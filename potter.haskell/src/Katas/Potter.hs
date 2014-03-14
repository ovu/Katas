module Katas.Potter where

import Test.Hspec

countNonZeroBooks :: [(Int, Int)] -> Int
countNonZeroBooks [] = 0 
countNonZeroBooks [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)] = 0 
countNonZeroBooks ( ( _, b ): xs ) = if b > 0
                                     then 1 + (countNonZeroBooks xs) 
                                     else countNonZeroBooks xs 

-- NonNeroBooksSpec
countNonZeroBooksSpec :: Spec
countNonZeroBooksSpec = do
  describe "countNonZeroBooks" $ do
    it "returns 0 when all non book is in the list" $ do
      countNonZeroBooks [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` 0
    it "returns 1 when one of the books in the serie was bought" $ do
      countNonZeroBooks [(1, 1), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` 1
    it "returns 2 when two books of the serie was bought" $ do
      countNonZeroBooks [(1, 1), (2, 1), (3, 0), (4, 0), (5,0)] `shouldBe` 2
    it "returns 5 when five books of the serie was bought" $ do
      countNonZeroBooks [(1, 1), (2, 1), (3, 1), (4, 1), (5,1)] `shouldBe` 5

decreaseNonZeroBooks :: [(Int, Int)] -> [(Int, Int)]
decreaseNonZeroBooks [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)] = [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)]
decreaseNonZeroBooks [] = []
decreaseNonZeroBooks ( (a, b): xs ) = if b > 0
                                      then (a, b -1) : decreaseNonZeroBooks xs
                                      else (a, b) : decreaseNonZeroBooks xs

-- decrease non zero books specifications
decreaseNonZeroBooksSpec :: Spec
decreaseNonZeroBooksSpec = do
  describe "decreaseNonZeroBooks" $ do
    it "returns does not change the list when there is no book" $ do
      decreaseNonZeroBooks [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)]
    it "returns the list of books decrease by one" $ do
      decreaseNonZeroBooks [(1, 1), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)]
    it "returns the list of books decrease by one when some books are more than once" $ do
      decreaseNonZeroBooks [(1, 1), (2, 4), (3, 2), (4, 3), (5,3)] `shouldBe` [(1, 0), (2, 3), (3, 1), (4, 2), (5,2)]

makeGroups :: [(Int, Int)] -> [Int]

makeGroups [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)] = [0]
makeGroups (xs) = (countNonZeroBooks xs): makeGroups(decreaseNonZeroBooks xs)

-- make groups specifications

makeGroupsSpec :: Spec
makeGroupsSpec  = do
  describe "makeGroups" $ do
    it "returns [0] when there is no book" $ do
      makeGroups[(1, 0), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` [0]
    it "returns the groups of books with zero at the end" $ do
      makeGroups[(1, 2), (2, 3), (3, 1), (4, 2), (5,1)] `shouldBe` [5, 3, 1, 0]

calculatePriceForGroup :: [Int] -> Double

calculatePriceForGroup [] = 0
calculatePriceForGroup (0: xs) = calculatePriceForGroup xs
calculatePriceForGroup (1: xs) = 8 + calculatePriceForGroup xs
calculatePriceForGroup (2: xs) = 2 * 8 * 0.95 + calculatePriceForGroup xs
calculatePriceForGroup (3: xs) = 3 * 8 * 0.90 + calculatePriceForGroup xs
calculatePriceForGroup (4: xs) = 4 * 8 * 0.80 + calculatePriceForGroup xs
calculatePriceForGroup (5:3: xs) = calculatePriceForGroup (4:4: xs)
calculatePriceForGroup (5: xs) = 5 * 8 * 0.75 + calculatePriceForGroup xs

-- calculate price for group specifications

calculatePriceForGroupSpec :: Spec
calculatePriceForGroupSpec  = do
  describe "calculate price for group" $ do
    it "returns 0 when there is no book" $ do
      calculatePriceForGroup [0] `shouldBe` 0 
    it "returns 8 when there is one book" $ do
      calculatePriceForGroup [1] `shouldBe` 8
    it "returns 51.2 when there is one group of five and one of 3" $ do
      calculatePriceForGroup [5, 3] `shouldBe` 51.2
    it "returns 81.2 when there is one group of two five and one of 3" $ do
      calculatePriceForGroup [5, 5, 3] `shouldBe` 81.2

calculatePrice :: [(Int, Int)] -> Double

calculatePrice xs = calculatePriceForGroup(makeGroups xs)

-- calculate price specifications

calculatePriceSpec :: Spec
calculatePriceSpec  = do
  describe "calculate price" $ do
    it "returns 8 when there is one book" $ do
      calculatePrice [(1, 1), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` 8 
    it "returns 16 when there are two books from the same exemplar" $ do
      calculatePrice [(1, 2), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` 16 
    it "returns 16 when there are two books from the same exemplar" $ do
      calculatePrice [(1, 2), (2, 2), (3, 2), (4, 1), (5,1)] `shouldBe` 51.2 
