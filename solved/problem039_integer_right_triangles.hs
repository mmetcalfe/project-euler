-- 
-- Problem 39: Integer right triangles
-- (Published on Friday, 14th March 2003, 06:00 pm; Solved by 39835)
-- 
--     If p is the perimeter of a right angle triangle with integral
-- 	length sides, {a,b,c}, there are exactly three solutions for p =
-- 	120.
-- 
--     {20,48,52}, {24,45,51}, {30,40,50}
-- 
--     For which value of p ≤ 1000, is the number of solutions
-- 	maximised?

import Data.List (maximumBy)
import Data.Function (on)

triangles p = [(a, b, (p - a - b)) | a <- [1..p], b <- [a..p], b <= p - a - b]

isRight (a, b, c) = a^2 + b^2 == c^2

rightTriangles p = filter isRight (triangles p)

main = do
	let n = 1000
	    rightTris = map rightTriangles [1..n]
	    maxSolsP = maximumBy (compare `on` (length . snd)) (zip [1..n] rightTris)
	print maxSolsP
	