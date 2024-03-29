module Primes where
-- module Primes (isFactor, isPrime, isPrimeFactor, primeFactors, primeFactorsMultiplicity) where

import Data.List (intersect)

isFactor :: Integer -> Integer -> Bool
isFactor n k = mod n k == 0

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n
    | n <= 0 = False
    | otherwise = not $ any (isFactor n) testRange
    where maxTest = min (ceiling $ sqrt (fromIntegral n)) (n-1)
          testRange = [2..maxTest]

primes :: [Integer]
primes = filter isPrime [1..]

isPrimeFactor :: Integer -> Integer -> Bool
isPrimeFactor n k = (isFactor n k) && (isPrime k)

firstPrimeFactor :: Integer -> Integer -> Integer
firstPrimeFactor n k
    | isPrimeFactor n k = k
    | k <= n = firstPrimeFactor n (k + 1)
    | otherwise = error $ (show n) ++ " has no prime factors >= " ++ (show k)

primeFactors :: Integer -> [Integer]
primeFactors n
    | n == 1 = []
    | n == f = [f]
    | otherwise =
      let r = factorOrder n f
      in f : primeFactors (div n (f^r))
    where f = firstPrimeFactor n 2

areCoprime :: Integer -> Integer -> Bool
areCoprime a b = null $ intersect (primeFactors a) (primeFactors b)

coprimes :: Integer -> [Integer]
coprimes k = filter (areCoprime k) [1..]

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
