--
-- Problem 32: Pandigital products
-- (Published on Friday, 6th December 2002, 06:00 pm; Solved by 38222)
--
--     We shall say that an n-digit number is pandigital if it makes
-- 	use of all the digits 1 to n exactly once; for example, the 5-digit
-- 	number, 15234, is 1 through 5 pandigital.
--
--     The product 7254 is unusual, as the identity, 39 × 186 = 7254,
-- 	containing multiplicand, multiplier, and product is 1 through 9
-- 	pandigital.
--
--     Find the sum of all products whose
-- 	multiplicand/multiplier/product identity can be written as a 1
-- 	through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure
-- 	to only include it once in your sum.

-- a * b = c
-- num digits must add to 9
-- cannot repeat digits
-- require a < b (symmetry)
-- upper limit (digits) to product?
--   - < 8 (must have a product)
--   - < 5 (99*99 = 9801)
--   - >= 4 (99*99 = 9801)
--   - == 4
--   - 1234 <= c <= 9876

import Data.List (sort, union)

numDigits :: Integer -> Integer
numDigits n =  1 + round (logBase 10 (fromIntegral n))

isPandigitalString :: String -> Bool
isPandigitalString s = sort s == "123456789"

isPandigitalProduct a b = isPandigitalString (show a ++ show b ++ show (a*b))

searchRange n = [1234 `div` n..9876 `div` n]

main = do
    let pairs = filter (uncurry isPandigitalProduct) [(a, b) | a <- [1..100], b <- searchRange a]
        products = map (uncurry (*)) pairs
    print $ zip pairs products
    print $ sum (union [] products)