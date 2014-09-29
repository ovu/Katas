module PrimeNumbers

type System.Int32 with 
    member x.IsDivisibleBy(divisor) = x % divisor = 0

type PrimeNumbers() = 
    member this.IsPrimeNumber(number:int) =
                      match number with
                        | 2 -> true
                        | 3 -> true
                        | numberToTest when numberToTest.IsDivisibleBy 2 -> false
                        | numberToTest when numberToTest.IsDivisibleBy 3 -> false
         
