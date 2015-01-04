--
-- Problem 24: Lexicographic permutations
-- (Published on Friday, 16th August 2002, 06:00 pm; Solved by 62517)
--
--     A permutation is an ordered arrangement of objects. For example,
-- 	3124 is one possible permutation of the digits 1, 2, 3 and 4. If
-- 	all of the permutations are listed numerically or alphabetically,
-- 	we call it lexicographic order. The lexicographic permutations of
-- 	0, 1 and 2 are:
--
--     012   021   102   120   201   210
--
--     What is the millionth lexicographic permutation of the digits 0,
-- 	1, 2, 3, 4, 5, 6, 7, 8 and 9?

import Data.List
import Data.Maybe

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialAbove :: Integer -> Integer
factorialAbove n = fromJust $ find ((>n) . factorial) [1 ..]

decomposeFactoradic :: Integer -> Integer -> [Integer]
decomposeFactoradic 0 _ = [0]
decomposeFactoradic k n =
    let (d, n') = divMod n (factorial k)
    in d : decomposeFactoradic (k - 1) n'

factoradic :: Integer -> [Integer]
factoradic n = decomposeFactoradic (factorialAbove n - 1) n

removeAt i l =
    let e = l !! fromInteger i
        (s1,_:s2) = splitAt (fromInteger i) l
        s' = s1 ++ s2
    in (e, s')

toPermutation :: [Integer] -> [a] -> [a]
toPermutation [] _ = []
toPermutation _ [] = []
toPermutation f s =
    let i:fs = padLeft (length s) 0 f
        (e, s') = removeAt i s
    in e : toPermutation fs s'

padLeft n e l = replicate (n - length l) e ++ l

main = do
    let n = 1000000
    let s = "0123456789"
    let f = factoradic (n - 1)
    print $ toPermutation f s
    -- print $ factorialAbove n
    -- print $ factorial k
    -- print $ factoradic 719
    -- print $ factoradic 720
    -- print $ f
    -- print $ map (padLeft (length s) 0 . factoradic) [0..5]
    print $ map (flip toPermutation s . factoradic) [0..5]
