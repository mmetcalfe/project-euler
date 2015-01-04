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

approxNthFib :: Integer -> Double
approxNthFib n = (phi ^ n) / sqrt 5

numDigits :: Double -> Integer
numDigits n = 1 + floor (logBase 10 n)

main = do
    let n = fromJust $ find ((>=1000) . numDigits . approxNthFib) [1..]
    print n
    print $ numDigits $ fromIntegral n
    print $ approxNthFib $ fromIntegral n
    print $ map (approxNthFib) [1..1475]
