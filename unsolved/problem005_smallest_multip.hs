--
-- Problem 5: Smallest multiple
-- (Published on Friday, 30th November 2001, 06:00 pm; Solved by 192935)
--
--     2520 is the smallest number that can be divided by each of the
--      numbers from 1 to 10 without any remainder.
--
--     What is the smallest positive number that is evenly divisible
--      (i.e. divisible with no remainder) by all of the numbers from 1 to
--      20?

--import solved.problem003_largest_prime_factor as problem3

--def product_of_occurrences(dict):
--    val = 1
--    for key in dict:
--        val *= key**dict[key]
--    return val

--def count_occurrences(list):
--    dict = {}
--    for k in list:
--        if k in dict:
--            dict[k] += 1
--        else:
--            dict[k] = 1
--    return dict

--def merge_dicts_max_values(dict1, dict2):
--    result = dict1.copy()
--    for key in dict2:
--        if key in dict1:
--            result[key] = max(dict1[key], dict2[key])
--        else:
--            result[key] = dict2[key]
--    return result


--n = 20
--answer_factors = {}
--for k in range(1, n + 1):
--    factors = problem3.prime_factors(k)
--    occurrences = count_occurrences(factors)
--    answer_factors = merge_dicts_max_values(answer_factors, occurrences)
--    #print(k, factors, occurrences, answer_factors)

--print(answer_factors)
--print('Answer:', product_of_occurrences(answer_factors))

isFactor :: Integer -> Integer -> Bool
isFactor n f = n `mod` f == 0

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = not (any (isFactor n) [2..n-1])

factors :: Integer -> [Integer]
factors n = filter (isFactor n) [1..n]

primeFactors :: Integer -> [Integer]
primeFactors n = filter isPrime (factors n)

-- -- a^b = c => log_a(c) = b
-- floorLog :: Integer -> Integer -> Integer
-- floorLog b v = floor (logBase (fromIntegral b) (fromIntegral v)::Double)

factorOrder' :: Integer -> Integer -> Integer -> Integer
factorOrder' n f k = if isFactor n v then factorOrder' n f k' else k
    where k' = k+1
          v  = f^k'
factorOrder :: Integer -> Integer -> Integer
factorOrder n f = factorOrder' n f 0

primeFactorsMultiplicity :: Integer -> [(Integer, Integer)]
primeFactorsMultiplicity n =
    let p = primeFactors n
        m = map (factorOrder n) p
    in zip p m

-- smallestMultiple :: Integer -> Integer
-- smallestMultiple n = map primeFactorsMultiplicity [1..n]

main :: IO()
main = do
    print $ factors 60
    print $ primeFactors 60
    print $ primeFactorsMultiplicity 60
    print $ map primeFactorsMultiplicity [2..20]