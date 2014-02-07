define(['Game'], function (Game) {

  describe ('Game specs', function () {
  	describe ('When a game is created using default constructor', function () {
        var game;

  		beforeEach (function () {
  			game = new Game();
  		});

		it('should contain two divisors', function () {
			expect(game.firstDivisor).toNotBe(undefined);
			expect(game.secondDivisor).toNotBe(undefined);
		});
		
		it('should throw exception for nextTurn()', function() {
			expect(function () { game.nextTurn(); }).toThrow();
		});
  	});

	describe('When Game is created using partially provided constructor', function (){

		it('should throw exception for nextTurn() when created only with first divisor', function() {
			var game = new Game(2);
			expect(function () { game.nextTurn(); }).toThrow();
		});

		it('should throw exception for nextTurn() when created only with second divisor', function() {
			var game = new Game(undefined, 3);
			expect(function () { game.nextTurn(); }).toThrow();
		});
	});

  	describe ('When a game is created using constructor with 3 and 5 divisors', function () {
        var game;
        var firstDivisor = 3;
        var secondDivisor = 5;

  		beforeEach (function () {
  			game = new Game(firstDivisor, secondDivisor);
  		});

		it('should contain passed divisors', function () {
			expect(game.firstDivisor).toBe(firstDivisor);
			expect(game.secondDivisor).toBe(secondDivisor);
		});

		it ('the first time nextTurn should return 1', function(){
			expect(game.nextTurn()).toBe(1);	
		});


		it ('the second time nextTurn should return 2', function(){
			game.nextTurn();
			expect(game.nextTurn()).toBe(2);	
		});

		it ('the third time nextTurn should return FIZZ', function(){
			game.nextTurn();
			game.nextTurn();
			expect(game.nextTurn()).toBe('FIZZ');	
		});

		it ('the fourth time nextTurn should return 4', function(){
			game.nextTurn();
			game.nextTurn();
			game.nextTurn();
			expect(game.nextTurn()).toBe(4);	
		});

		it ('the fifth time nextTurn should return BUZZ', function(){
			game.nextTurn();
			game.nextTurn();
			game.nextTurn();
			game.nextTurn();
			expect(game.nextTurn()).toBe('BUZZ');	
		});
  	});

	describe('When Game is created with any divisors', function (){

		it('should return FIZZ for second turn with 2  and 3 divisors', function (){
			var game = new Game(2,3);
			game.nextTurn();
			expect(game.nextTurn()).toBe('FIZZ');	
		});

		it('should throw exception for nextTurn() when created with first divisor equals to 0', function() {
			var game = new Game(0, 3);
			expect(function () { game.nextTurn(); }).toThrow('Please provide valid divisors');
		});

		it('should throw exception for nextTurn() when created with second divisor equals to 0', function() {
			var game = new Game(3, 0);
			expect(function () { game.nextTurn(); }).toThrow(new Error('Please provide valid divisors'));
		});

		it('should throw exception for nextTurn() when created with negative first divisor', function() {
			var game = new Game(-10, 3);
			expect(function () { game.nextTurn(); }).toThrow('Please provide valid divisors');
		});

		it('should throw exception for nextTurn() when created with negative second divisor', function() {
			var game = new Game(3, -10);
			expect(function () { game.nextTurn(); }).toThrow('Please provide valid divisors');
		});
	});

  });

});
