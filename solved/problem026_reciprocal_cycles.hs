-- 
-- Problem 26: Reciprocal cycles
-- (Published on Friday, 13th September 2002, 06:00 pm; Solved by 44723)
-- 
--     A unit fraction contains 1 in the numerator. The decimal
--  representation of the unit fractions with denominators 2 to 10 are
--  given:
-- 
--         ^{1}/_{2}        =      0.5
--     ^{1}/_{3}        =    0.(3)
--     ^{1}/_{4}        =     0.25
--     ^{1}/_{5}        =      0.2
--     ^{1}/_{6}        =   0.1(6)
--     ^{1}/_{7}        =  0.(142857)
--     ^{1}/_{8}        =    0.125
--     ^{1}/_{9}        =    0.(1)
--     ^{1}/_{10}       =      0.1
-- 
-- 
--     Where 0.1(6) means 0.166666..., and has a 1-digit recurring
--  cycle. It can be seen that ^{1}/_{7} has a 6-digit recurring cycle.
-- 
--     Find the value of d < 1000 for which ^{1}/_{d} contains the
--  longest recurring cycle in its decimal fraction part.

import Data.List
import Data.Function
import Primes

decDiv :: Integer -> Integer -> [Integer]
decDiv n d
    | n < d = decDiv (n * 10) d
    | n `mod` d == 0 = [n `div` d]
    | n > d =
        let (q, r) = divMod n d
        in q : decDiv r d

decDivLoop :: [(Integer, Integer)] -> Integer -> Integer -> ([Integer], [(Integer, Integer)])
decDivLoop l n d
    | frac `elem` l = ([], l')
    | n < d = decDivLoop l' (n * 10) d
    | n `mod` d == 0 = ([n `div` d], l')
    | n > d =
        let (q, r) = divMod n d
            p = decDivLoop l' r d
        in (q : fst p, snd p) 
    where frac = (n, d)
          l' = frac : l

toNumString :: [Integer] -> String
toNumString = ("0."++) . concatMap show

main = do
    let k = 1000
        decimalExpansion = decDivLoop [] 1
        repeats e = (head . snd) e == (last . snd) e
        -- Note: Should remove non-repeating head of cycle before comparing
        longest = maximumBy (compare `on` (length . snd)) (filter repeats $ map decimalExpansion [2..k])
    print $ (length . fst) longest
    print $ fst longest
    print $ snd longest
    print $ (snd . head . snd) longest
    -- mapM_ print $ filter repeats $ map decimalExpansion [2..100]
    -- mapM_ (putStrLn . toNumString . (take 30) . (decDivLoop 1)) [2..100]
    -- mapM_ (print . decimalExpansion) [2..50]
    -- print $ takeWhile (<100) primes
