-- 
-- Problem 43: Sub-string divisibility
-- (Published on Friday, 9th May 2003, 06:00 pm; Solved by 31806)
-- 
--     The number, 1406357289, is a 0 to 9 pandigital number because it
-- 	is made up of each of the digits 0 to 9 in some order, but it also
-- 	has a rather interesting sub-string divisibility property.
-- 
--     Let d_{1} be the 1^{st} digit, d_{2} be the 2^{nd} digit, and so
-- 	on. In this way, we note the following:
-- 
--         - d_{2}d_{3}d_{4}=406 is divisible by 2
--     - d_{3}d_{4}d_{5}=063 is divisible by 3
--     - d_{4}d_{5}d_{6}=635 is divisible by 5
--     - d_{5}d_{6}d_{7}=357 is divisible by 7
--     - d_{6}d_{7}d_{8}=572 is divisible by 11
--     - d_{7}d_{8}d_{9}=728 is divisible by 13
--     - d_{8}d_{9}d_{10}=289 is divisible by 17
-- 
-- 
--     Find the sum of all 0 to 9 pandigital numbers with this
-- 	property.

import Primes
import Data.List

slice from to l = take (to - from + 1) $ drop (from - 1) l

substringDiv from to n s = read (slice from to s) `mod` n == 0

fullSubstringDiv s = all (\i -> substringDiv (i + 2) (i+4) (primes !! i) s) [0..6]

main = do
    let strings = filter fullSubstringDiv (permutations "0123456789")
        nums = map read strings :: [Integer]
    print nums
    print $ sum nums