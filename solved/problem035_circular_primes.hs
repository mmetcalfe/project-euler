--
-- Problem 35: Circular primes
-- (Published on Friday, 17th January 2003, 06:00 pm; Solved by 47573)
--
--     The number, 197, is called a circular prime because all
-- 	rotations of the digits: 197, 971, and 719, are themselves prime.
--
--     There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13,
-- 	17, 31, 37, 71, 73, 79, and 97.
--
--     How many circular primes are there below one million?

-- include 2
-- only use digits: 1, 3, 7, 9
-- <= 6 digits
-- 4^1 + 4^2 + 4^3 + 4^4 + 4^5 + 4^6 = 5460 numbers to test

import Control.Monad
import Data.List
import Primes

rotations :: [a] -> [[a]]
rotations xs = init (zipWith (++) (tails xs) (inits xs))

isCircular :: Integer -> Bool
isCircular n = all (isPrime . read) (rotations (show n))

circularNDigits n = let l = map (read . concat) (replicateM n ["1", "3", "7", "9"])
                    in filter isCircular l

circularNDigitsAndBelow 1 = [2, 3, 5, 7]
circularNDigitsAndBelow n = circularNDigits n ++ circularNDigitsAndBelow (n-1)

main = do
  print $ length $ circularNDigitsAndBelow 6
