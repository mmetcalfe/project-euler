module Primes (isFactor, isPrime, isPrimeFactor) where

isFactor :: Integer -> Integer -> Bool
isFactor n k = mod n k == 0

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = not $ any (isFactor n) testRange
    where maxTest = min (ceiling $ sqrt (fromIntegral n)) (n-1)
          testRange = [2..maxTest]

isPrimeFactor :: Integer -> Integer -> Bool
isPrimeFactor n k = (isFactor n k) && (isPrime k)

firstPrimeFactor :: Integer -> Integer -> Integer
firstPrimeFactor n k
    | isPrimeFactor n k = k
    | k <= n = firstPrimeFactor n (k + 1)

primeFactors :: Integer -> [Integer]
primeFactors n
    | n == f = f : []
    | otherwise = f : primeFactors (div n f)
    where f = firstPrimeFactor n 1

factorOrder' :: Integer -> Integer -> Integer -> Integer
factorOrder' n f k = if isFactor n v then factorOrder' n f k' else k
    where k' = k+1
          v  = f^k'
factorOrder :: Integer -> Integer -> Integer
factorOrder n f = factorOrder' n f 0

primeFactorsMultiplicity :: Integer -> [(Integer, Integer)]
primeFactorsMultiplicity n =
    let p = primeFactors n
        m = map (factorOrder n) p
    in zip p m