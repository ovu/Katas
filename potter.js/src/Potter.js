"use strict";

var Basket = function () {
	var numberOfBooks = {};
	var discounts = [1, 0.95, 0.9, 0.8, 0.75];

	this.addBooks = function (id, count) {
    if (Object.keys(numberOfBooks).indexOf("" + id) >= 0) {
      numberOfBooks[id] += count;
    } else {
      numberOfBooks[id] = count;
    }
	};

	this.getPrice = function () {
    var groupOfBooks = this.makeGroups();
    var price = 0;
    groupOfBooks.forEach(function (numberOfBooksInGroup) {
      price += numberOfBooksInGroup * 8 * discounts[numberOfBooksInGroup - 1];
    });
		return price;
	};
  
  this.makeGroups = function () {
    var groups = [];
    this.makeGroupForList(groups);
    return groups;
  };

  this.makeGroupForList = function (list) {
    var numberOfBooksInGroup = 0;
    
    Object.keys(numberOfBooks).forEach(function (id) {
      if (numberOfBooks[id] > 0) {
        numberOfBooksInGroup++;
        numberOfBooks[id]--;
      }
    });

    if (list.length > 0) {
      if (numberOfBooksInGroup === 3) {
        if (list[list.length - 1] === 5) {
          list[list.length - 1] = 4;
          numberOfBooksInGroup = 4;
        }
      }
    }

    if (numberOfBooksInGroup > 0) {
      list.push(numberOfBooksInGroup);
      this.makeGroupForList(list);
    }
  };
};
