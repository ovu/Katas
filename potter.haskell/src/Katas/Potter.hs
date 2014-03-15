module Katas.Potter where

import Test.Hspec

-- FUNCTION decreaseNumberOfBooks
-- Decrease the number of books for every exemplar in one when it has at least one book
-- Example: decreaseNumberOfBooks (1, 1) returns (1, 0)
-- Example: decreaseNumberOfBooks (1, 0) returns (1, 0)
decreaseNumberOfBooks :: [(Int, Int)] -> [(Int, Int)]

decreaseNumberOfBooks [] = []
decreaseNumberOfBooks ( (a, b): xs ) = if b > 0
                                      then (a, b -1) : decreaseNumberOfBooks xs
                                      else (a, b) : decreaseNumberOfBooks xs

-- Specifications for the function decreaseNumberOfBooks
decreaseNumberOfBooksSpec :: Spec
decreaseNumberOfBooksSpec = do
  describe "decreaseNumberOfBooks" $ do
    it "returns does not change the list when there is no book" $ do
      decreaseNumberOfBooks [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)]
    it "returns the list of books decrease by one" $ do
      decreaseNumberOfBooks [(1, 1), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` [(1, 0), (2, 0), (3, 0), (4, 0), (5,0)]
    it "returns the list of books decrease by one when some books are more than once" $ do
      decreaseNumberOfBooks [(1, 1), (2, 4), (3, 2), (4, 3), (5,3)] `shouldBe` [(1, 0), (2, 3), (3, 1), (4, 2), (5,2)]
    it "returns the list of books decrease by one when the list of books is incomplete" $ do
      decreaseNumberOfBooks [(1, 1)] `shouldBe` [(1, 0)]

-- FUNCTION makeGroupOfBooks
-- Return the groups of books in a list
-- Example: makeGroupOfBooks [(1, 1), (2, 1)] returns [2]
makeGroupOfBooks :: [(Int, Int)] -> [Int]

makeGroupOfBooks [] = [0]
makeGroupOfBooks (xs) = if lengthNonZeroBooks == 0
                        then [0]
                        else (lengthNonZeroBooks): makeGroupOfBooks(decreaseNumberOfBooks nonZeroBooks)
                        where
                          nonZeroBooks = filter (\ (_, b) -> b > 0) xs
                          lengthNonZeroBooks = length nonZeroBooks

-- Specifications for the function makeGroupOfBooks
makeGroupOfBooksSpec :: Spec
makeGroupOfBooksSpec  = do
  describe "makeGroupOfBooks" $ do
    it "returns [0] when there is no book" $ do
      makeGroupOfBooks[(1, 0), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` [0]
    it "returns the groups of books with zero at the end" $ do
      makeGroupOfBooks[(1, 2), (2, 3), (3, 1), (4, 2), (5,1)] `shouldBe` [5, 3, 1, 0]
    it "returns the groups of books when the list of books is incomplete" $ do
      makeGroupOfBooks[(1, 2), (2, 3)] `shouldBe` [2, 2, 1, 0]

-- FUNCTION calculatePriceForGroupOfBooks
-- Calculate the lowest price for a list of different books
-- See the specifications for examples 
calculatePriceForGroupOfBooks :: [Int] -> Double

calculatePriceForGroupOfBooks [] = 0
calculatePriceForGroupOfBooks (0: xs) = calculatePriceForGroupOfBooks xs
calculatePriceForGroupOfBooks (1: xs) = 8 + calculatePriceForGroupOfBooks xs
calculatePriceForGroupOfBooks (2: xs) = 2 * 8 * 0.95 + calculatePriceForGroupOfBooks xs
calculatePriceForGroupOfBooks (3: xs) = 3 * 8 * 0.90 + calculatePriceForGroupOfBooks xs
calculatePriceForGroupOfBooks (4: xs) = 4 * 8 * 0.80 + calculatePriceForGroupOfBooks xs
calculatePriceForGroupOfBooks (5:3: xs) = calculatePriceForGroupOfBooks (4:4: xs)
calculatePriceForGroupOfBooks (5: xs) = 5 * 8 * 0.75 + calculatePriceForGroupOfBooks xs
calculatePriceForGroupOfBooks (_:_) = error "Invalid group of books"

-- Specificatons for function calculatePriceForGroupOfBooks 
calculatePriceForGroupOfBooksSpec :: Spec
calculatePriceForGroupOfBooksSpec  = do
  describe "calculate price for group" $ do
    it "returns 0 when there is no book" $ do
      calculatePriceForGroupOfBooks [0] `shouldBe` 0 
    it "returns 8 when there is one book" $ do
      calculatePriceForGroupOfBooks [1] `shouldBe` 8
    it "returns 51.2 when there is one group of five and one of 3" $ do
      calculatePriceForGroupOfBooks [5, 3] `shouldBe` 51.2
    it "returns 81.2 when there is one group of two five and one of 3" $ do
      calculatePriceForGroupOfBooks [5, 5, 3] `shouldBe` 81.2

-- FUNCTION calculatePriceForBooks
-- Calculate the lowest price for a list of books
-- See the specifications for examples
calculatePriceForBooks :: [(Int, Int)] -> Double

calculatePriceForBooks xs = calculatePriceForGroupOfBooks(makeGroupOfBooks xs)

-- Specifications for calculatePriceForBooks
calculatePriceForBooksSpec :: Spec
calculatePriceForBooksSpec  = do
  describe "calculate price" $ do
    it "returns 8 when there is one book" $ do
      calculatePriceForBooks [(1, 1), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` 8 
    it "returns 16 when there are two books from the same exemplar" $ do
      calculatePriceForBooks [(1, 2), (2, 0), (3, 0), (4, 0), (5,0)] `shouldBe` 16 
    it "returns 16 when there are two books from the same exemplar" $ do
      calculatePriceForBooks [(1, 2), (2, 2), (3, 2), (4, 1), (5,1)] `shouldBe` 51.2 
