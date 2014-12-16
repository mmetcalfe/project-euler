--
-- Problem 9: Special Pythagorean triplet
-- (Published on Friday, 25th January 2002, 06:00 pm; Solved by 180235)
--
--     A Pythagorean triplet is a set of three natural numbers, a < b <
-- 	c, for which,
--  a^{2} + b^{2} = c^{2}
--     For example, 3^{2} + 4^{2} = 9 + 16 = 25 = 5^{2}.
--
--     There exists exactly one Pythagorean triplet for which a + b + c
-- 	= 1000.
-- Find the product abc.

check :: Integer -> Integer -> Integer -> Bool
check n a b = a^2 + b^2 == (n - a - b)^2

getBforA :: Integer -> Integer -> Integer
getBforA n a = ((n^2 `div` 2) - n * a) `div` (n - a)

-- solve' :: Integer -> Integer -> (Integer, Integer, Integer)
-- solve' n a
--     | a > 1000 = (0, 0, 0)
--     | otherwise =
--     let b = getBforA n a
--     in if check n a b then (a, b, 1000 - a - b) else solve' n (a + 1)
-- solve :: Integer -> (Integer, Integer, Integer)
-- solve n = solve' n 1

main :: IO()
main = do
    let n = 1000
        rawCandidates = map (\x -> (x, getBforA n x)) [1..n - 1]
        abTuples = [(a,b) | (a,b) <- filter ((>0) . snd) rawCandidates, a < b]
        solutions = filter (uncurry (check n)) abTuples
    print rawCandidates
    print abTuples
    print solutions
    if solutions == []
        then print "No solution"
        else let (a,b) = head solutions
                 c = n - a - b
                 in do
                   print $ (a, b, c)
                   print $ a * b * c
