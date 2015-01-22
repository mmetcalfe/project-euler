module Divisors where

isDivisor :: Integer -> Integer -> Bool
isDivisor n = (==0) . mod n

divisorPairs :: Integer -> [(Integer,Integer)]
divisorPairs n = map (\k -> (k, div n k)) $ filter (isDivisor n) [1 .. floor $ sqrt (fromIntegral n)]

divisors :: Integer -> [Integer]
divisors n = concatMap (\(a,b) ->
                    if (a == b)
                      then [a]
                      else [a, b]) (divisorPairs n)

properDivisors :: Integer -> [Integer]
properDivisors n = concatMap (\(a,b) ->
                    if (a == b) || (b == n)
                      then [a]
                      else [a, b]) (divisorPairs n)

isAbundant :: Integer -> Bool
isAbundant n = ((>n) . sum . properDivisors) n

isDeficient :: Integer -> Bool
isDeficient n = ((<n) . sum . properDivisors) n

isPerfect :: Integer -> Bool
isPerfect n = ((==n) . sum . properDivisors) n

abundants :: [Integer]
abundants = filter isAbundant [1..]

deficients :: [Integer]
deficients = filter isDeficient [1..]

perfects :: [Integer]
perfects = filter isPerfect [1..]
