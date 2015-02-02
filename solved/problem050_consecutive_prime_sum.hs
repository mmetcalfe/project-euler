-- 
-- Problem 50: Consecutive prime sum
-- (Published on Friday, 15th August 2003, 06:00 pm; Solved by 33320)
-- 
--     The prime 41, can be written as the sum of six consecutive
--  primes:
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
--     This is the longest sum of consecutive primes that adds to a
--  prime below one-hundred.
-- 
--     The longest sum of consecutive primes below one-thousand that
--  adds to a prime, contains 21 terms, and is equal to 953.
-- 
--     Which prime, below one-million, can be written as the sum of the
--  most consecutive primes?

import Data.List
import Primes

consecutiveSums :: Integer -> [Integer] -> [Integer]
-- consecutiveSums k l = foldr (zipWith (+)) l (take k $ tail $ tails l)
consecutiveSums k l =
    let cumul = scanl (+) 0 l
    in zipWith (-) (genericDrop k cumul) cumul

interleavelists :: [[a]] -> [a]
interleavelists = concat . transpose

main = do
    let n = 10^6 -- 10^6
        primeRange = takeWhile (<n) primes
        greatestPrime = last primeRange -- 999983
        longestSeq = fst $ last $  takeWhile ((<n) . snd) $ map (\x -> (x, (sum . flip take primes) x)) [1..]
        evenLengthSums = filter (even . snd) $ zip (map (:[]) (scanl1 (+) primeRange)) [1..] -- even length sums must start with 2
        oddLengthSums = map (\k -> (consecutiveSums k (tail primeRange), k)) [3, 5..]
        sums = interleavelists [evenLengthSums, take longestSeq oddLengthSums] :: [([Integer], Integer)]
    print $ last $ filter (not . null . fst) $ map (\(a, b) -> (take 1  $ filter isPrime $ filter (<n) a, b)) sums