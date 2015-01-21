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

champernowneFracString :: String
champernowneFracString = concatMap show [1..]
champernowneString = "0." ++ champernowneFracString

numDigits :: Integer -> Integer
numDigits n = 1 + fromIntegral (length (takeWhile ((<=n) . (10^)) [1..])) :: Integer

numBelowNDigits :: Integer -> Integer
numBelowNDigits n = 10 ^ (n - 1) - 1
numNDigitIntegers 0 = 1
numNDigitIntegers n = numBelowNDigits (n + 1) - numBelowNDigits n

champernowneEndIndexOf :: Integer -> Integer
champernowneEndIndexOf n =
    let digits = numDigits n
        lowerPlaces = sum $ map (\x -> x * numNDigitIntegers x) [0 .. digits - 1]
    in lowerPlaces + digits * (n - numBelowNDigits digits) -- + (1 - digits)

champernowneIndexOf :: Integer -> Integer
champernowneIndexOf n = champernowneEndIndexOf n + (1 - numDigits n)

champernowneIntegerAtIndex :: Integer -> (Integer, Integer)
champernowneIntegerAtIndex n =
        let digitCounts = map (\x -> x * numNDigitIntegers x) [1..]
            digitsBeforeList = takeWhile (<n) (map sum (inits digitCounts))
            numDigits = (fromIntegral $ length digitsBeforeList) :: Integer
            digitsBefore = last digitsBeforeList
            digitsAfter = n - digitsBefore + (numDigits - 1)
            (nDigitIntegerIndex, digitOfNum) = fromIntegral digitsAfter `divMod` fromIntegral numDigits
            intAtIndex = numBelowNDigits numDigits + nDigitIntegerIndex
        in (intAtIndex, (numDigits - 1) - digitOfNum)


digitOfInteger :: Integer -> Integer -> Integer
digitOfInteger k i = (k `div` 10^i) `mod` 10

champernowneDigitAtIndex :: Integer -> Integer
champernowneDigitAtIndex n = uncurry digitOfInteger (champernowneIntegerAtIndex n)

main = do
--         putStrLn "champernowneIndexOf: "
--         print $ zip [1..] (map champernowneIndexOf [1..102])
--         putStrLn "champernowneDigitAtIndex: "
--         print $ zip [1..] (map champernowneDigitAtIndex [1..200])
--         putStrLn "champernowneFracString: "
--         print $ zip [1..] (take 200 champernowneFracString)

    let d = champernowneDigitAtIndex
    print $ d 1 * d 10 * d 100 * d 1000 * d 10000 * d 100000 * d 1000000
