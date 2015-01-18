--
-- Problem 31: Coin sums
-- (Published on Friday, 22nd November 2002, 06:00 pm; Solved by 44625)
--
--     In England the currency is made up of pound, £, and pence, p,
-- 	and there are eight coins in general circulation:
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
--     It is possible to make £2 in the following way:
-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
--     How many different ways can £2 be made using any number of
-- 	coins?

-- num solutions to integer linear program:
-- 1a + 2b + 5c + 10d + 20e + 50f + 100g + 200h = 200
-- a <= 200
-- b <= 100
-- c <= 40
-- d <= 20
-- e <= 10
-- f <= 50
-- g <= 2
-- h <= 1
-- a,b,c,d,e,f,g,h >= 0

type Change = [(Integer, Integer)]

denominations = [2, 5, 10, 20, 50, 100, 200]

emptyChange :: Change
emptyChange = map ((,) 0) denominations

value :: Change -> Integer
value c = sum $ map (uncurry (*)) c

nextChange :: Integer -> Change -> Change
nextChange k [] = []
nextChange k ((v,d):r)
    | v < (k `div` d) = (v + 1, d) : r
    | otherwise = (0, d) : nextChange k r

changeSets k = iterate (nextChange k) emptyChange

main = do
    let k = 200
        numSets = product (map (fromIntegral . div k) denominations)
    print numSets
    print $ take numSets (changeSets k)
