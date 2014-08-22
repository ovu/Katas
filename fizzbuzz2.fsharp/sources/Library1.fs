module Fizzbuzz

open NUnit.Framework
open System
open FluentAssertions

type System.Int32  with
    member x.IsDivisibleBy3 = x % 3 = 0
    member x.IsDivisibleBy5 = x % 5 = 0
    member x.IsDivisibleBy15 = x.IsDivisibleBy3 && x.IsDivisibleBy5

type FizzBuzz() = 
    member this.GiveMeAnswer( number:int)  = 
        match number with
        | number when number.IsDivisibleBy15 -> "FizzBuzz"
        | number when number.IsDivisibleBy3 -> "Fizz"
        | number when number.IsDivisibleBy5 -> "Buzz"
        | number -> string number
        
    member this.GiveMeAnswersFromList(numbers) = 
        match numbers with
        | [] -> []
        | x::xs -> [this.GiveMeAnswer(x)] @ this.GiveMeAnswersFromList(xs) 


[<Test>]
let TryWithArray () =
    let numbers = [ 1..5 ]
    let expectedAnswers = [ "1"; "2"; "Fizz"; "4"; "Buzz" ]
    let game = new FizzBuzz()
    let answers = game.GiveMeAnswersFromList(numbers)
    Assert.AreEqual(expectedAnswers, answers)
    
[<Test>]
let WhenNumberIsOneItShouldReturnOne () =
    let game = new FizzBuzz()    
    Assert.AreEqual(game.GiveMeAnswer(1), "1");

[<Test>]
let WhenNumberIsThreeItShouldReturnFizz () =
    let game = new FizzBuzz()    
    Assert.AreEqual(game.GiveMeAnswer(3), "Fizz");
    
[<Test>]
let WhenNumberIsFiveItShouldReturnBuzz () =
    let game = new FizzBuzz()
    Assert.AreEqual(game.GiveMeAnswer(5),"Buzz");

[<Test>]
let WhenNumberIsFivteenItShouldReturnFizzBuzz () =
    let game = new FizzBuzz()
    Assert.AreEqual(game.GiveMeAnswer(15),"FizzBuzz");