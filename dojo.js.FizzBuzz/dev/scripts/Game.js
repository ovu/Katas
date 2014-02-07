define([], function () {

	var game;

	game = function (firstDivisor, secondDivisor) {
		var self = this;
		var turn = 0;

		self.firstDivisor = firstDivisor === undefined  ?  null : firstDivisor;
		self.secondDivisor = secondDivisor === undefined  ?  null : secondDivisor;

		self.nextTurn = function (){

			if(self.firstDivisor === null || self.secondDivisor === null){
				throw new Error('Please provide divisors');
			};

			if(self.firstDivisor <= 0 || self.secondDivisor <= 0){
				throw new Error('Please provide valid divisors');				
			};

			turn++;

			if (turn % firstDivisor == 0)
			{
				return "FIZZ";
			}

			if (turn % 5 == 0)
			{
				return "BUZZ";
			}

			return turn;
		};

	};
	return game;
});
