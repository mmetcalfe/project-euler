--
-- Problem 10: Summation of primes
-- (Published on Friday, 8th February 2002, 06:00 pm; Solved by 165325)
--
--     The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
--     Find the sum of all the primes below two million.
--
-- <p class="note">Note: This problem has been changed recently, please
-- 	check that you are using the right parameters.</p>

primeSieve :: Integer -> [Integer] -> Integer -> [Integer]
primeSieve n l k
  | n < k = l
  | otherwise =
    let factorBound = ceiling $ sqrt $ (fromIntegral k) :: Integer
        isFactor = (==0) . (k`mod`)
        isPrime = not $ any isFactor (filter (<=factorBound) l)
        nextL = if isPrime then l ++ [k] else l
      in primeSieve n nextL (k+2)
primesBelow :: Integer -> [Integer]
primesBelow n = primeSieve n [2, 3, 5] 7

main = do
  let l =  primesBelow 1000
  print $ l
  print $ sum l
