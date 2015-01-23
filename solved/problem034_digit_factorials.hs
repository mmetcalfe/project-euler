-- 
-- Problem 34: Digit factorials
-- (Published on Friday, 3rd January 2003, 06:00 pm; Solved by 52157)
-- 
--     145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- 
--     Find the sum of all numbers which are equal to the sum of the
-- 	factorial of their digits.
-- 
--     Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.Digits

factorial 0 = 1
factorial n = n * factorial (n - 1)

isDigitFactorialSum n = ((== n) . sum . (map factorial) . (digits 10)) n

main = do
	print $ sum $ filter isDigitFactorialSum [3..10^6]
