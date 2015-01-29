-- 
-- Problem 48: Self powers
-- (Published on Friday, 18th July 2003, 06:00 pm; Solved by 66706)
-- 
--     The series, 1^{1} + 2^{2} + 3^{3} + ... + 10^{10} = 10405071317.
-- 
--     Find the last ten digits of the series, 1^{1} + 2^{2} + 3^{3} +
-- 	... + 1000^{1000}.

powMod :: Integer -> Integer -> Integer
powMod n k = foldr (\x y -> x*y `mod` 10^10) 1 (replicate (fromIntegral k) n)

main = do
    print $ (sum $ map (\x -> powMod x x) [1..1000]) `mod` 10^10
