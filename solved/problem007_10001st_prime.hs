-- 
-- Problem 7: 10001st prime
-- (Published on Friday, 28th December 2001, 06:00 pm; Solved by 207019)
-- 
--     By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
-- 	we can see that the 6th prime is 13.
-- 
--     What is the 10 001st prime number?

module Main (main) where
import Primes (isPrime)

nextPrime :: Integer -> Integer
nextPrime n = if isPrime n then n else nextPrime (n + 1)

nthPrime' :: Integer -> Integer -> Integer
nthPrime' n k = if n == 0 then k - 1 else nthPrime' (n - 1) (nextPrime k + 1)
nthPrime :: Integer -> Integer
nthPrime n = nthPrime' n 1

main :: IO ()
main = do
    print $ nextPrime 32
    print $ nthPrime 1
    print $ nthPrime 2
    print $ nthPrime 3
    print $ nthPrime 5
    print $ nthPrime 12
    print $ nthPrime 10001
