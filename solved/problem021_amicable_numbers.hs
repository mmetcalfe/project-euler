--
-- Problem 21: Amicable numbers
-- (Published on Friday, 5th July 2002, 06:00 pm; Solved by 77726)
--
--     Let d(n) be defined as the sum of proper divisors of n (numbers
--   less than n which divide evenly into n).
--
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable
--   pair and each of a and b are called amicable numbers.
--
--     For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11,
--   20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors
--   of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
--
--     Evaluate the sum of all the amicable numbers under 10000.

import Primes

divisors :: Integer -> [Integer]
divisors n = filter ((== 0) . mod n) [1 .. div n 2]


amicableSibling = sum . divisors


isAbundant :: Integer -> Integer -> Bool
isAbundant n = (>n) . sum . divisors

isLowerAmicable n =
    let s = amicableSibling n
    in if s <= n
         then False
         else n == amicableSibling s

main = do
    let n = 10000
    let la = filter isLowerAmicable [1..n]
    let pairs = map (\x -> (x, amicableSibling x)) la
    print $ la
    print $ pairs
    print $ sum $ map (\(a,b) -> a+b) pairs
