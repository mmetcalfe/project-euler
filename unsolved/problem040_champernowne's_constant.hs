-- 
-- Problem 40: Champernowne's constant
-- (Published on Friday, 28th March 2003, 06:00 pm; Solved by 44505)
-- 
--     An irrational decimal fraction is created by concatenating the
-- 	positive integers:
-- 
--     0.123456789101112131415161718192021...
-- 
--     It can be seen that the 12^{th} digit of the fractional part is
-- 	1.
-- 
--     If d_{n} represents the n^{th} digit of the fractional part,
-- 	find the value of the following expression.
-- 
--     d_{1} × d_{10} × d_{100} × d_{1000} × d_{10000} × d_{100000} ×
-- 	d_{1000000}

import Data.List

champernowneFracString = concatMap show [1..]
champernowneString = "0." ++ champernowneFracString

numDigits :: Integer -> Integer
numDigits n = 1 + fromIntegral (length (takeWhile ((<=n) . (10^)) [1..])) :: Integer 

numBelowNDigits :: Integer -> Integer
numBelowNDigits n = 10 ^ (n - 1) - 1
numNDigits n = numBelowNDigits (n + 1) - numBelowNDigits n

champernowneIndexOf n = 
	let digits = numDigits n
	    lowerPlaces = sum $ map (\x -> x * numNDigits x) [0 .. digits - 1]
	in lowerPlaces + digits * (n - numBelowNDigits digits)

champernowneIntegerAtIndex :: Integer -> Integer
champernowneIntegerAtIndex n =
	let counts = map (\x -> x * numNDigits x) [1..]
	    before = takeWhile (<=n) (map sum (inits counts))
	    digits = (fromIntegral $ length before) :: Integer
	    numBefore = last before
	    indexDigits = ceiling $ fromIntegral (n - numBefore) / fromIntegral digits
	in numBefore + indexDigits


main = do
	print $ zip [1..] (map champernowneIndexOf [1..102])
	print $ zip [1..] (map champernowneIntegerAtIndex [1..200])
	print $ zip [1..] (take 200 champernowneFracString)
