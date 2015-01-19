--
-- Problem 37: Truncatable primes
-- (Published on Friday, 14th February 2003, 06:00 pm; Solved by 40412)
--
--     The number 3797 has an interesting property. Being prime itself,
-- 	it is possible to continuously remove digits from left to right,
-- 	and remain prime at each stage: 3797, 797, 97, and 7. Similarly we
-- 	can work from right to left: 3797, 379, 37, and 3.
--
--     Find the sum of the only eleven primes that are both truncatable
-- 	from left to right and right to left.
--
--     NOTE: 2, 3, 5, and 7 are not considered to be truncatable
-- 	primes.

import Primes

isRightTruncatable p
  | isPrime p =
    let t = p `div` 10
    in if t == 0
      then True
      else isRightTruncatable t
  | otherwise = False

isLeftTruncatable p
  | p < 10 = isPrime p
  | isPrime p =
    let t = read (tail (show p)) :: Integer
    in isLeftTruncatable t
  | otherwise = False

isTruncatable p = isRightTruncatable p && isLeftTruncatable p

main = do
  let l = take 11 $ filter isTruncatable [10..]
  print l
  print $ sum l
