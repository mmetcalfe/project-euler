-- 
-- Problem 137: Fibonacci golden nuggets
-- (Published on Friday, 12th January 2007, 06:00 pm; Solved by 3047)
-- 
--     Consider the infinite polynomial series A_{F}(x) = xF_{1} +
-- 	x^{2}F_{2} + x^{3}F_{3} + ..., where F_{k} is the kth term in the
-- 	Fibonacci sequence: 1, 1, 2, 3, 5, 8, ... ; that is, F_{k} =
-- 	F_{k−1} + F_{k−2}, F_{1} = 1 and F_{2} = 1.
-- 
--     For this problem we shall be interested in values of x for which
-- 	A_{F}(x) is a positive integer.
-- 
--         Surprisingly A_{F}(1/2)      =  (1/2).1 + (1/2)^{2}.1 +
-- 	(1/2)^{3}.2 + (1/2)^{4}.3 + (1/2)^{5}.5 + ...
--                  =  1/2 + 1/4 + 2/8 + 3/16 + 5/32 + ...
--                  =        2
-- 
-- 
--     The corresponding values of x for the first five natural numbers
-- 	are shown below.
-- 
--             *x* *A_{F}(x)*
--        √2−1       1
--         1/2       2
--     (√13−2)/3         3
--     (√89−5)/8         4
--     (√34−3)/5         5
-- 
-- 
--     We shall call A_{F}(x) a golden nugget if x is rational, because
-- 	they become increasingly rarer; for example, the 10th golden nugget
-- 	is 74049690.
-- 
--     Find the 15th golden nugget.
