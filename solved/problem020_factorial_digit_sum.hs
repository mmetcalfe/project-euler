-- 
-- Problem 20: Factorial digit sum
-- (Published on Friday, 21st June 2002, 06:00 pm; Solved by 110226)
-- 
--     n! means n × (n − 1) × ... × 3 × 2 × 1
-- 
--     For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0
-- 	+ 0 = 27.
-- 
--     Find the sum of the digits in the number 100!

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main = do
    let s = factorial 100
    print $ sum (map (read . (:[])) (show s) :: [Integer])
