module FizzBuzzGame

type FizzBuzz() = 
    member this.GiveMeAnswer = function
        | number when (number % 15) = 0 -> "fizzbuzz"
        | number when (number % 3) = 0 -> "fizz"
        | number when (number % 5) = 0 -> "buzz"
        | number -> string number
