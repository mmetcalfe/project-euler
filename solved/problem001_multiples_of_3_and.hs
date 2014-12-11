{-
 Problem 1: Multiples of 3 and 5
 (Published on Friday, 5th October 2001, 06:00 pm; Solved by 324709)

     If we list all the natural numbers below 10 that are multiples
        of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

     Find the sum of all the multiples of 3 or 5 below 1000.
-}

--print(sum([k for k in range(1000) if k % 3 is 0 or k % 5 is 0]))

--main = do
--      putStrLn "Hello, what's your name?"
--      name <- getLine
--      putStrLn ("Hey " ++ name ++ ", you rock!")

sum3Or5 :: Integer -> Integer
sum3Or5 1 = 0
sum3Or5 n
        | mod k 5 == 0 = k + sum3Or5 k
        | mod k 3 == 0 = k + sum3Or5 k
        | otherwise    = sum3Or5 k
        where k = n - 1

main = do
        print (sum3Or5 10)
        print (sum3Or5 1000)
