--
-- Problem 17: Number letter counts
-- (Published on Friday, 17th May 2002, 06:00 pm; Solved by 80213)
--
--     If the numbers 1 to 5 are written out in words: one, two, three,
-- 	four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in
-- 	total.
--
--     If all the numbers from 1 to 1000 (one thousand) inclusive were
-- 	written out in words, how many letters would be used?
--
--
--     *NOTE:* Do not count spaces or hyphens. For example, 342 (three
-- 	hundred and forty-two) contains 23 letters and 115 (one hundred and
-- 	fifteen) contains 20 letters. The use of "and" when writing out
-- 	numbers is in compliance with British usage.


spell 0 = ""
spell 1 = "one"
spell 2 = "two"
spell 3 = "three"
spell 4 = "four"
spell 5 = "five"
spell 6 = "six"
spell 7 = "seven"
spell 8 = "eight"
spell 9 = "nine"
spell 10 = "ten"
spell 11 = "eleven"
spell 12 = "twelve"
spell 13 = "thirteen"
spell 14 = "fourteen"
spell 15 = "fifteen"
spell 16 = "sixteen"
spell 17 = "seventeen"
spell 18 = "eighteen"
spell 19 = "nineteen"
spell 20 = "twenty"
spell 30 = "thirty"
spell 40 = "forty"
spell 50 = "fifty"
spell 60 = "sixty"
spell 70 = "seventy"
spell 80 = "eighty"
spell 90 = "ninety"
spell n
  | n >= 1000 = spell d ++ "thousand" ++ spell r
  | n >= 100 = spell d ++ "hundred" ++ (if r > 0 then "and" else "") ++ spell r
  | n >= 10 = spell (d * 10) ++ spell r
  | otherwise = spell d
  where
    s = show n
    r = read $ tail s
    d = read $ [head $ s] :: Integer


main = print $ sum $ map (length . spell) [1..1000]
