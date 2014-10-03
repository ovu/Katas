module PrimeNumbers

type System.Int32 with 
    member x.IsDivisibleBy(divisor) = x % divisor = 0

let rec IsPrimeNumberStartingWith (startNumber:int, number:int) = 
                        match compare startNumber number with
                          | 0 -> true
                          | result when result < 0 && number % startNumber = 0 -> false
                          | _ -> IsPrimeNumberStartingWith( startNumber + 1 , number)

let IsPrimeNumber(number:int) = IsPrimeNumberStartingWith(2, number)

         
