--
-- Problem 15: Lattice paths
-- (Published on Friday, 19th April 2002, 06:00 pm; Solved by 97366)
--
--     Starting in the top left corner of a 2×2 grid, and only being
-- 	able to move to the right and down, there are exactly 6 routes to
-- 	the bottom right corner.
--
-- <img alt="" src="project/images/p015.gif"/>
--     How many such routes are there through a 20×20 grid?


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

choose :: Integer -> Integer -> Integer
choose n k = factorial n `div` (factorial k * factorial (n - k))

-- (2n choose n)
latticePaths :: Integer -> Integer
latticePaths n = choose (2*n) n

main :: IO ()
main = do
    let n = 20
    print $ latticePaths n
