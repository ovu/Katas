var BankAccount = function (startingAmount) {

	this.amount = startingAmount;

	this.getButtons = function(cash){

		if (this.amount === 0) {return []};

		var possibleButtons = [50, 500, 2500, 10000, 250000, 10000000];

        function overLimit(limit) {
            return cash > limit;
        }

        return possibleButtons.filter( overLimit ).concat([cash]); 
	};
};