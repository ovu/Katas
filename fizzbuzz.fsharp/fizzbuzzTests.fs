module FizzBuzzGame.Tests

open FizzBuzzGame
open NUnit.Framework

let game = new FizzBuzz()

[<Test>]
let When_the_number_is_3_it_should_return_fizz() =
    Assert.AreEqual("fizz", game.GiveMeAnswer(3))

[<Test>]
let When_the_number_is_divisible_by_3_it_should_return_fizz() =
    Assert.AreEqual("fizz", game.GiveMeAnswer(6))

[<Test>]
let When_the_number_is_5_it_should_return_buzz() =
    Assert.AreEqual("buzz", game.GiveMeAnswer(5))

[<Test>]
let When_the_number_is_divisible_by_5_it_should_return_buzz() =
    Assert.AreEqual("buzz", game.GiveMeAnswer(10))

[<Test>]
let When_the_number_is_divisible_by_3_and_5_it_should_return_fizzbuzz() =
    Assert.AreEqual("fizzbuzz", game.GiveMeAnswer(15))

[<Test>]
let When_the_number_is_not_divisible_by_3_and_5_it_should_return_the_same_number() =
    Assert.AreEqual("2", game.GiveMeAnswer(2))
