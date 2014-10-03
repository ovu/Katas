module PrimeNumbers

type System.Int32 with 
    member x.IsDivisibleBy(divisor) = x % divisor = 0

let rec LeastDivisorStartingWith (startNumber:int , number:int) =
                        match (startNumber, number) with
                           | (divisor, number) when number % divisor = 0 -> divisor
                           | (divisor, number) when divisor*2 > number -> number
                           | (_, _) -> LeastDivisorStartingWith(startNumber + 1, number) 

let IsPrimeNumber(number:int) = LeastDivisorStartingWith(2, number) = number
