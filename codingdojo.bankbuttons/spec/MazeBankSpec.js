describe("Mazen Bank tests", function (){
	describe("Buttons test", function() {
	   
describe("When we generate Maze Bank with zero amount", function() {
	   	  var bankAccount;
	   	  beforeEach(function () {
	   	  	  bankAccount = new BankAccount(0);
	   	  });
			it("should return empty list", function(){
				expect(bankAccount.getButtons(0)).toEqual([]);
				});
		});



	   describe("When we generate Maze Bank amounts", function() {
	   	  var bankAccount;
	   	  beforeEach(function () {
	   	  	  bankAccount = new BankAccount(1);
	   	  });
			it("should return the buttons for 50 and 100 Euros", function(){
				expect(bankAccount.getButtons(100)).toEqual([50, 100]);
			});
		    it("should return the buttons for 50 and 110 Euros", function(){
				expect(bankAccount.getButtons(110)).toEqual([50, 110]);
			});				
		    it("should return the buttons for 50, 500 Euros", function(){
				expect(bankAccount.getButtons(500)).toEqual([50, 500]);
			});				
		    it("should return the buttons for 50, 500, 501 Euros", function(){
				expect(bankAccount.getButtons(501)).toEqual([50, 500, 501]);
			});				
		    it("should return the buttons for 50, 500, 2500, 2501 Euros", function(){
				expect(bankAccount.getButtons(2501)).toEqual([50, 500, 2500, 2501]);
			});				
		    it("should return the buttons for 50, 500, 2500, 10000, 10001 Euros", function(){
				expect(bankAccount.getButtons(10001)).toEqual([50, 500, 2500, 10000, 10001]);
			});				
		    it("should return the buttons for 50, 500, 2500, 10000, 250000, 250001 Euros", function(){
				expect(bankAccount.getButtons(250001)).toEqual([50, 500, 2500, 10000, 250000, 250001]);
			});				
		    it("should return the buttons for 50, 500, 2500, 10000, 250000, 10000001 Euros", function(){
				expect(bankAccount.getButtons(10000001)).toEqual([50, 500, 2500, 10000, 250000, 10000000, 10000001]);
			});

		});
	});
});