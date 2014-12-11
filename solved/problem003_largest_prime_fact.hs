-- 
-- Problem 3: Largest prime factor
-- (Published on Friday, 2nd November 2001, 06:00 pm; Solved by 194923)
-- 
--     The prime factors of 13195 are 5, 7, 13 and 29.
-- 
--     What is the largest prime factor of the number 600851475143 ?
-- 
-- Note: This problem has been changed recently, please check that you
--      are using the right number.

main = do
    print $ primeFactors 60
    print $ primeFactors 600851475143
    print $ (maximum . primeFactors) 600851475143
    print $ 600851475143 == (product $ primeFactors 600851475143)

isFactor :: Integer -> Integer -> Bool
isFactor n k = mod n k == 0

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = not $ any (isFactor n) testRange
    where maxTest = min (ceiling $ sqrt (fromIntegral n)) (n-1)
          testRange = [2..maxTest]

isPrimeFactor :: Integer -> Integer -> Bool
isPrimeFactor n k = (isFactor n k) && (isPrime k)

firstPrimeFactor :: Integer -> Integer -> Integer
firstPrimeFactor n k
    | isPrimeFactor n k = k
    | k <= n = firstPrimeFactor n (k + 1)

primeFactors :: Integer -> [Integer]
primeFactors n
    | n == f = f : []
    | otherwise = f : primeFactors (div n f)
    where f = firstPrimeFactor n 1
