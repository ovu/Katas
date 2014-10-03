module PrimeNumbers

type System.Int32 with 
    member x.IsDivisibleBy(divisor) = x % divisor = 0

let rec LeastDivisorStartingWith (startNumber:int , number:int) =
                        match (startNumber, number) with
                           | (divisor, number) when number.IsDivisibleBy divisor -> divisor
                           | (divisor, number) when divisor*2 > number -> number
                           | (_, _) -> LeastDivisorStartingWith(startNumber + 1, number) 

let IsPrimeNumber(number:int) =
                  match (number) with 
                    | 1 -> false
                    | _ -> LeastDivisorStartingWith(2, number) = number
