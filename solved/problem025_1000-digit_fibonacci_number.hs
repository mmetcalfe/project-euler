-- 
-- Problem 25: 1000-digit Fibonacci number
-- (Published on Friday, 30th August 2002, 06:00 pm; Solved by 84798)
-- 
--     The Fibonacci sequence is defined by the recurrence relation:
--
--       F_{n} = F_{n−1} + F_{n−2}, where F_{1} = 1 and F_{2} = 1.
--
--     Hence the first 12 terms will be:
--
--       F_{1} = 1
--       F_{2} = 1
--       F_{3} = 2
--       F_{4} = 3
--       F_{5} = 5
--       F_{6} = 8
--       F_{7} = 13
--       F_{8} = 21
--       F_{9} = 34
--       F_{10} = 55
--       F_{11} = 89
--       F_{12} = 144
--
--     The 12th term, F_{12}, is the first term to contain three
-- 	digits.
-- 
--     What is the first term in the Fibonacci sequence to contain 1000
-- 	digits?

import Data.List
import Data.Maybe

phi :: Double
phi = (1.0 + sqrt 5) / 2.0
psi :: Double
psi = 1.0 - phi

approxNthFib :: Integer -> Double
approxNthFib n = (phi ^ n) / sqrt 5

numDigits :: Double -> Integer
numDigits n = 1 + floor (logBase 10 n)

approxNumDigitsNthFib :: Integer -> Integer
approxNumDigitsNthFib n = 1 + floor (fromIntegral n * logBase 10.0 phi - logBase 10 (sqrt 5))

log10 = logBase 10
numDigitsNthFib :: Double -> Double
numDigitsNthFib n = 1 + n*log10 phi + log10 (1 - (psi/phi)**n) - log10 (sqrt 5)

main = do
    let nc = fromJust $ find ((>=1000) . numDigits . approxNthFib) [1..]
    let na = fromJust $ find ((>=1000) . approxNumDigitsNthFib) [1..]
    let ne = fromJust $ find ((>=1000) . numDigitsNthFib) [1..]
    print nc
    print na
    print ne
    print $ approxNumDigitsNthFib $ fromIntegral nc
    print $ approxNumDigitsNthFib $ na
    let r = [ne-10..ne+10]
    print $ zip r $ map (approxNumDigitsNthFib . round) r
    print $ zip r $ map (numDigitsNthFib) r
