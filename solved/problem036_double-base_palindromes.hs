--
-- Problem 36: Double-base palindromes
-- (Published on Friday, 31st January 2003, 06:00 pm; Solved by 50486)
--
--     The decimal number, 585 = 1001001001_{2} (binary), is
-- 	palindromic in both bases.
--
--     Find the sum of all numbers, less than one million, which are
-- 	palindromic in base 10 and base 2.
--
--     (Please note that the palindromic number, in either base, may
-- 	not include leading zeros.)

import Numeric

isPalindromeAtBase :: Integer -> Integer -> Bool
isPalindromeAtBase b n =
  let s = showIntAtBase b (head . show) n ""
  in s == reverse s

isDoublePalindrome :: Integer -> Bool
isDoublePalindrome n = isPalindromeAtBase 2 n && isPalindromeAtBase 10 n

main = do
    let l = filter isDoublePalindrome [1..1000000]
    print $ sum l
