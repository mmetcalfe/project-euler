-- 
-- Problem 4: Largest palindrome product
-- (Published on Friday, 16th November 2001, 06:00 pm; Solved by 178192)
-- 
--     A palindromic number reads the same both ways. The largest
--      palindrome made from the product of two 2-digit numbers is 9009 =
--      91 × 99.
-- 
--     Find the largest palindrome made from the product of two 3-digit
--      numbers.


isPalindrome :: Integer -> Bool
isPalindrome n = (show n) == (reverse . show $ n)

maxPalindrome :: Integer -> Integer
maxPalindrome n = maximum [ x * y | x <- [1..n], y <- [1..n], isPalindrome (x * y)]

main = do
    print $ maxPalindrome 999
