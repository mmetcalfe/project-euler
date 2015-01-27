-- 
-- Problem 41: Pandigital prime
-- (Published on Friday, 11th April 2003, 06:00 pm; Solved by 37170)
-- 
--     We shall say that an n-digit number is pandigital if it makes
-- 	use of all the digits 1 to n exactly once. For example, 2143 is a
-- 	4-digit pandigital and is also prime.
-- 
--     What is the largest n-digit pandigital prime that exists?

import Data.List
import Primes
import Pandigital

main = do
    let perms = concatMap (permutations . flip take "123456789") [1..9]
        pans = filter isNDigitPandigital perms
        primes = filter isPrime (map read pans :: [Integer])
    print primes
    print $ maximum primes
