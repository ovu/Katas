describe("When buying books", function() {
  var basket;
  beforeEach(function(){
    basket = new Basket();
  });
  describe("When one book is bought", function(){
    it("should say the price is 8 EUR", function () {
      basket.addBooks(1,1);
      expect(basket.getPrice()).toBe(8);
    });
  });
  describe("When two the same books are bought", function(){
    it("should say the price is 16 EUR", function () {
      basket.addBooks(1,2);
      expect(basket.getPrice()).toBe(16);
    }); 
  });

  describe("When two different books are bought", function(){
    it("the price should be 15.20 EUR", function () {
      basket.addBooks(1, 1);
      basket.addBooks(2, 1);
      expect(basket.getPrice()).toBe(15.20);
    });
  });

  describe("When three different books are bought", function () {
    it("should return the price 21.60 EUR", function () {
      basket.addBooks(1, 1);
      basket.addBooks(2, 1);
      basket.addBooks(3, 1);
      expect(basket.getPrice()).toBe(21.60);
    });
  });
  describe("When three different books are bought", function () {
    it("should return the price 25.60 EUR", function () {
      basket.addBooks(1, 1);
      basket.addBooks(2, 1);
      basket.addBooks(3, 1);
      basket.addBooks(4, 1);
      expect(basket.getPrice()).toBe(25.60);
    });
  });
  describe("When three different books are bought", function () {
    it("should return the price 30 EUR", function () {
      basket.addBooks(1, 1);
      basket.addBooks(2, 1);
      basket.addBooks(3, 1);
      basket.addBooks(4, 1);
      basket.addBooks(5, 1);

      expect(basket.getPrice()).toBe(30);
    });
  });

});


describe("When grouping the books", function () {
  var basket;
  beforeEach(function () {
    basket = new Basket();
  });

  describe("When in the basket there are two different books", function () {
    it("Should return one group of two books", function () {
      basket.addBooks(1, 1);
      basket.addBooks(2, 1);
      expect(basket.makeGroups()).toEqual([2]);
    });
    it("Should return a group of 5 and one of 1 when there are 6 books", function () {
      basket.addBooks(1, 1);
      basket.addBooks(2, 1);
      basket.addBooks(3, 1);
      basket.addBooks(4, 1);
      basket.addBooks(5, 1);
      basket.addBooks(1, 1);
      expect(basket.makeGroups()).toEqual([5, 1]);
    });
  });
  
  describe("When there are 8 different books in the basket", function () {
    it("Should return a group of 4 and one of 4 when there are 8 books", function () {
      basket.addBooks(1, 1);
      basket.addBooks(2, 1);
      basket.addBooks(3, 1);
      basket.addBooks(4, 1);
      basket.addBooks(5, 1);
      basket.addBooks(1, 1);
      basket.addBooks(2, 1);
      basket.addBooks(3, 1);
      expect(basket.makeGroups()).toEqual([4, 4]);
    });
  });
});

