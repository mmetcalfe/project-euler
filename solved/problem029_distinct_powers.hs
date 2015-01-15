--
-- Problem 29: Distinct powers
-- (Published on Friday, 25th October 2002, 06:00 pm; Solved by 56540)
--
--     Consider all integer combinations of a^{b} for 2 ≤ a ≤ 5 and 2 ≤
-- 	b ≤ 5:
-- 2^{2}=4, 2^{3}=8, 2^{4}=16, 2^{5}=32
-- 3^{2}=9, 3^{3}=27, 3^{4}=81, 3^{5}=243
-- 4^{2}=16, 4^{3}=64, 4^{4}=256, 4^{5}=1024
-- 5^{2}=25, 5^{3}=125, 5^{4}=625, 5^{5}=3125
--
--     If they are then placed in numerical order, with any repeats
-- 	removed, we get the following sequence of 15 distinct terms:
--
--     4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
--
--     How many distinct terms are in the sequence generated by a^{b}
-- 	for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?

-- only powers of a can cause collisions with a

-- only count numbers without integer roots

-- count all powers of each number <= 100

import Data.List

hasRoots :: Integer -> Integer -> Bool
hasRoots k n
  | r < 1.9 = False
  | rf ^ k == n = True
  | rc ^ k == n = True
  | otherwise = hasRoots (k + 1) n
  where r = fromIntegral n**(1.0/fromIntegral k)
        rc = ceiling r
        rf = floor r

hasNoRoots :: Integer -> Bool
hasNoRoots = not . hasRoots 2

powers :: Integer -> [Integer]
powers n = map (n^) [1..]

powersBelow :: Integer -> Integer -> [Integer]
powersBelow _ 1 = [1]
powersBelow k n = takeWhile ((<k).(n^)) [1..]

actualPowersBelow :: Integer -> Integer -> [Integer]
actualPowersBelow _ 1 = [1]
actualPowersBelow k n = takeWhile (<k) (powers n)

partitionPowers :: Integer -> [(Integer, [Integer])]
partitionPowers n =
  let noRoots = filter hasNoRoots [2..n]
      partitions = map (powersBelow (n+1)) noRoots
  in zip noRoots partitions

uniquePowers n l = foldl union [] (map (\x -> map (*x) [2..n]) l)

main = do
    let n = 100
        parts = partitionPowers n
        pows = map snd parts
        uniq = map (uniquePowers n) pows
        num = sum (map length uniq)
--     print parts
--     print pows
--     print uniq
    print num

--     let ref = sort $ union [] [a^b | a <- [2..n], b <- [2..n]]
--     putStrLn "Reference:"
--     print $ ref
--     print $ length $ ref