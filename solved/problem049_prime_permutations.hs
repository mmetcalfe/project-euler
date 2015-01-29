-- 
-- Problem 49: Prime permutations
-- (Published on Friday, 1st August 2003, 06:00 pm; Solved by 30985)
-- 
--     The arithmetic sequence, 1487, 4817, 8147, in which each of the
-- 	terms increases by 3330, is unusual in two ways: (i) each of the
-- 	three terms are prime, and, (ii) each of the 4-digit numbers are
-- 	permutations of one another.
-- 
--     There are no arithmetic sequences made up of three 1-, 2-, or
-- 	3-digit primes, exhibiting this property, but there is one other
-- 	4-digit increasing sequence.
-- 
--     What 12-digit number do you form by concatenating the three
-- 	terms in this sequence?

import Primes
import Data.List

fourDigitPrimes = takeWhile (<10000) $ dropWhile (<1000) primes

perms n = sort $ filter isPrime $ nub $ filter (>=1000) $ map read (permutations n)

length4Subsequences = (filter ((==3) . length)) . subsequences

isArithmetic l =
    let d = drop 1 $ zipWith (-) l (0:l)
    in all (== head d) d

main = do
    let digitSets = nub $ map (sort . show) fourDigitPrimes
    print $ filter (not . null) $ map (filter isArithmetic . length4Subsequences . perms) digitSets
