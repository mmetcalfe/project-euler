--
-- Problem 38: Pandigital multiples
-- (Published on Friday, 28th February 2003, 06:00 pm; Solved by 34350)
--
--     Take the number 192 and multiply it by each of 1, 2, and 3:
-- 192 × 1 = 192
--
-- 192 × 2 = 384
--
-- 192 × 3 = 576
--     By concatenating each product we get the 1 to 9 pandigital,
-- 	192384576. We will call 192384576 the concatenated product of 192
-- 	and (1,2,3)
--
--     The same can be achieved by starting with 9 and multiplying by
-- 	1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the
-- 	concatenated product of 9 and (1,2,3,4,5).
--
--     What is the largest 1 to 9 pandigital 9-digit number that can be
-- 	formed as the concatenated product of an integer with (1,2, ... ,
-- 	n) where n > 1?

import Data.List
import Data.Maybe

isPandigital n = length n == 9 && sort n == "123456789"

numDigits :: Integer -> Integer
numDigits n = 1 + floor (logBase 10 (fromIntegral n))

concatenatedProduct :: Integer -> Maybe Integer
concatenatedProduct n =
	let products = map (show . (*n)) [1..9]
	    concats = map concat (drop 1 (inits products))
	in case (filter isPandigital concats) of
		p:_ -> Just (read p :: Integer)
		[]  -> Nothing

main = do
	let prods = filter (/= Nothing) $ map concatenatedProduct [1..10^5]
	print $ maximum (map fromJust prods)
