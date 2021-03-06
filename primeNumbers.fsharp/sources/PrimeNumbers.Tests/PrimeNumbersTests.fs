﻿module PrimeNumbersTests

open NUnit.Framework
open PrimeNumbers
open System

[<Test>]
let WhenNumberOneIsTestedItShouldReturnFalse() =
  let result = IsPrimeNumber(1);
  Assert.IsFalse(result);

[<Test>]
let WhenNumberLessThanOneIsTestedItShouldReturnInvalidOperationException() =
  let a = 1
  Assert.Throws<InvalidOperationException>(fun() -> IsPrimeNumber(-1) |> ignore ) |> ignore

[<Test>]
let WhenNumberTwoIsTestedItShouldReturnTrue() =
  let result = IsPrimeNumber(2);
  Assert.IsTrue(result);

[<Test>]
let WhenNumberThreeIsTestedItShouldReturnTrue() =
  let result = IsPrimeNumber(3);
  Assert.IsTrue(result);

[<Test>]
let WhenNumberFourIsTestedItShouldReturnFalse() =
  let result = IsPrimeNumber(4);
  Assert.IsFalse(result);

[<Test>]
let WhenNumberSixIsTestedItShouldReturnFalse() =
  let result = IsPrimeNumber(6);
  Assert.IsFalse(result);

[<Test>]
let WhenNumberElevenIsTestedItShouldReturnTrue() =
  let result = IsPrimeNumber(11);
  Assert.IsTrue(result);
