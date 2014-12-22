-- 
-- Problem 14: Longest Collatz sequence
-- (Published on Friday, 5th April 2002, 06:00 pm; Solved by 116893)
-- 
--     The following iterative sequence is defined for the set of
-- 	positive integers:
-- 
--     n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
-- 
--     Using the rule above and starting with 13, we generate the
-- 	following sequence:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--     It can be seen that this sequence (starting at 13 and finishing
-- 	at 1) contains 10 terms. Although it has not been proved yet
-- 	(Collatz Problem), it is thought that all starting numbers finish
-- 	at 1.
-- 
--     Which starting number, under one million, produces the longest
-- 	chain?
-- 
--     *NOTE:* Once the chain starts the terms are allowed to go above
-- 	one million.

import Data.List
import Data.Ord

collatz :: Integer -> Integer
collatz n
    | even n    = div n 2
    | otherwise = 3*n + 1

hailstone :: Integer -> [Integer]
hailstone 1 = [1]
hailstone n = n : (hailstone . collatz) n

mapInputs :: (Integer -> Integer) -> [Integer] -> [(Integer, Integer)]
mapInputs f l = zip l (map f l)

main :: IO ()
main = do
    let n = 1000000
    print $ collatz n
    print $ hailstone n
    print $ maximumBy (comparing snd) (mapInputs (fromIntegral . length . hailstone) [1..n-1])
