--
-- Problem 23: Non-abundant sums
-- (Published on Friday, 2nd August 2002, 06:00 pm; Solved by 54710)
--
--     A perfect number is a number for which the sum of its proper
-- 	divisors is exactly equal to the number. For example, the sum of
-- 	the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which
-- 	means that 28 is a perfect number.
--
--     A number n is called deficient if the sum of its proper divisors
-- 	is less than n and it is called abundant if this sum exceeds n.
--
--      A number whose proper divisors are less than the number is
-- 	called deficient and a number whose proper divisors exceed the
-- 	number is called abundant.
--
--     As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16,
-- 	the smallest number that can be written as the sum of two abundant
-- 	numbers is 24. By mathematical analysis, it can be shown that all
-- 	integers greater than 28123 can be written as the sum of two
-- 	abundant numbers. However, this upper limit cannot be reduced any
-- 	further by analysis even though it is known that the greatest
-- 	number that cannot be expressed as the sum of two abundant numbers
-- 	is less than this limit.
--
--     Find the sum of all the positive integers which cannot be
-- 	written as the sum of two abundant numbers.

isDivisor n = (==0) . (mod n)

divisorPairs :: Integer -> [(Integer,Integer)]
divisorPairs n = map (\k -> (k, div n k)) $ filter (isDivisor n) [1 .. floor $ sqrt (fromIntegral n)]

divisors n = concat $ map (\(a,b) ->
                    if a == b
                      then [a]
                      else if b == n
                        then [a]
                        else [a, b]) (divisorPairs n)

isAbundant :: Integer -> Bool
isAbundant n = ((>n) . sum . divisors) n

abundants :: [Integer]
abundants = filter isAbundant [1..]

nextAbundantSum n =
  let l = takeWhile (<=n) abundants


main = do
  print $ takeWhile (<=28123) abundants
