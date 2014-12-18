--
-- Problem 12: Highly divisible triangular number
-- (Published on Friday, 8th March 2002, 06:00 pm; Solved by 112336)
--
--     The sequence of triangle numbers is generated by adding the
-- 	natural numbers. So the 7^{th} triangle number would be 1 + 2 + 3 +
-- 	4 + 5 + 6 + 7 = 28. The first ten terms would be:
--
--     1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
--     Let us list the factors of the first seven triangle numbers:
--       * 1*: 1
--       * 3*: 1,3
--       * 6*: 1,2,3,6
--       *10*: 1,2,5,10
--       *15*: 1,3,5,15
--       *21*: 1,3,7,21
--       *28*: 1,2,4,7,14,28
--     We can see that 28 is the first triangle number to have over
-- 	five divisors.
--
--     What is the value of the first triangle number to have over five
-- 	hundred divisors?

module Main (main) where
import Primes

triangle :: Integer -> Integer
triangle k = k * (k+1) `div` 2

main =
  let k = 664
      n = if odd k then k else div k 2
      m = if odd k then div (k+1) 2 else (k+1)
      l = concat $ map primeFactorsMultiplicity [m, n]
  in do
    print $ triangle k
    print $ primeFactorsMultiplicity n
    print $ primeFactorsMultiplicity m
    print $ l