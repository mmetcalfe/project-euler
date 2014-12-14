-- 
-- Problem 6: Sum square difference
-- (Published on Friday, 14th December 2001, 06:00 pm; Solved by 241484)
-- 
--     The sum of the squares of the first ten natural numbers is,
-- 1^{2} + 2^{2} + ... + 10^{2} = 385
--     The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^{2} = 55^{2} = 3025
--     Hence the difference between the sum of the squares of the first
-- 	ten natural numbers and the square of the sum is 3025 − 385 = 2640.
-- 
--     Find the difference between the sum of the squares of the first
-- 	one hundred natural numbers and the square of the sum.

sumSquares :: Integer -> Integer
sumSquares n = sum [k^2 | k <- [1 .. n]]

squaredSum :: Integer -> Integer
squaredSum n = sum [1 .. n] ^ 2

sumSquaredDifference :: Integer -> Integer
sumSquaredDifference n = squaredSum n - sumSquares n

main :: IO()
main = do
    let n = 100
    print $ sumSquaredDifference n