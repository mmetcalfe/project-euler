-- 
-- Problem 16: Power digit sum
-- (Published on Friday, 3rd May 2002, 06:00 pm; Solved by 121767)
-- 
--     2^{15} = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 =
-- 	26.
-- 
--     What is the sum of the digits of the number 2^{1000}?

main :: IO ()
main = print $ sum $ map (read . (:[])) (show (2^1000))