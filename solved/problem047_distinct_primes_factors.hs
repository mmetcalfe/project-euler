-- 
-- Problem 47: Distinct primes factors
-- (Published on Friday, 4th July 2003, 06:00 pm; Solved by 31186)
-- 
--     The first two consecutive numbers to have two distinct prime
-- 	factors are:
-- 
--     14 = 2 × 7
-- 15 = 3 × 5
-- 
--     The first three consecutive numbers to have three distinct prime
-- 	factors are:
-- 
--     644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.
-- 
--     Find the first four consecutive integers to have four distinct
-- 	prime factors. What is the first of these numbers?

import Data.List
import Primes

primeGaps = filter (not . isPrime . head) $ groupBy (\a b -> isPrime a == isPrime b) [1..] 

numDistinctPrimeFactors :: Integer -> Integer
numDistinctPrimeFactors = fromIntegral . length . primeFactors

dropLast n l = let k = length l - fromIntegral n in take k l

main = do
    let k = 4 :: Integer
        factorsTest = (==k) . numDistinctPrimeFactors
        consecutiveTest n = all factorsTest [n..(n+k-1)]
        longGaps = filter (not . null) $ map (dropLast (k - 1)) primeGaps
        first = head $ head $ filter (not . null) $ map (filter consecutiveTest) longGaps
        nums = [first..(first+k-1)]
    print $ zip nums (map primeFactors nums)
