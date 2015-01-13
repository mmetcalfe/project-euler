--
-- Problem 30: Digit fifth powers
-- (Published on Friday, 8th November 2002, 06:00 pm; Solved by 59917)
--
--     Surprisingly there are only three numbers that can be written as
-- 	the sum of fourth powers of their digits:
-- 1634 = 1^{4} + 6^{4} + 3^{4} + 4^{4}
-- 8208 = 8^{4} + 2^{4} + 0^{4} + 8^{4}
-- 9474 = 9^{4} + 4^{4} + 7^{4} + 4^{4}
--     As 1 = 1^{4} is not a sum it is not included.
--
--     The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
--     Find the sum of all the numbers that can be written as the sum
-- 	of fifth powers of their digits.

import Data.List
import Data.Maybe
import Data.Char

maxDigits n = fromJust $ find (\k -> k*9^n <= 10^k) [1..]

digitPowerSum n k = sum $ map (^n) (map digitToInt (show k))

main = do
  let n = 5
      m = maxDigits n
  print $ sum $ filter (\k -> digitPowerSum n k == k) [2..10^m]
