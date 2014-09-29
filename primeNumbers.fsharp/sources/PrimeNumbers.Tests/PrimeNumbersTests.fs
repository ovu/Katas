module PrimeNumbersTests

open NUnit.Framework
open System
open PrimeNumbers

[<Test>]
let WhenNumberTwoIsTestedItShouldReturnTrue() =
  let primeNumberTester = new PrimeNumbers();
  let result = primeNumberTester.IsPrimeNumber(2);
  Assert.IsTrue(result);

[<Test>]
let WhenNumberThreeIsTestedItShouldReturnTrue() =
  let primeNumberTester = new PrimeNumbers();
  let result = primeNumberTester.IsPrimeNumber(3);
  Assert.IsTrue(result);

[<Test>]
let WhenNumberFourIsTestedItShouldReturnFalse() =
  let primeNumberTester = new PrimeNumbers();
  let result = primeNumberTester.IsPrimeNumber(4);
  Assert.IsFalse(result);

[<Test>]
let WhenNumberSixIsTestedItShouldReturnFalse() =
  let primeNumberTester = new PrimeNumbers();
  let result = primeNumberTester.IsPrimeNumber(6);
  Assert.IsFalse(result);
