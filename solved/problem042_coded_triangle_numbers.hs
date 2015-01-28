-- 
-- Problem 42: Coded triangle numbers
-- (Published on Friday, 25th April 2003, 06:00 pm; Solved by 42383)
-- 
--     The n^{th} term of the sequence of triangle numbers is given by,
-- 	t_{n} = ½n(n+1); so the first ten triangle numbers are:
-- 
--     1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- 
--     By converting each letter in a word to a number corresponding to
-- 	its alphabetical position and adding these values we form a word
-- 	value. For example, the word value for SKY is 19 + 11 + 25 = 55 =
-- 	t_{10}. If the word value is a triangle number then we shall call
-- 	the word a triangle word.
-- 
--     Using [words.txt](project/resources/p042_words.txt) (right click
-- 	and 'Save Link/Target As...'), a 16K text file containing nearly
-- 	two-thousand common English words, how many are triangle words?

-- "A","ABILITY","ABLE","ABOUT","ABOVE"

module Main (main) where

import System.IO
import qualified Data.List.Split as Split
import Data.List
import Data.Char
import Figurates

readWords :: String -> [String]
readWords contents = map (filter (/='"')) (Split.splitOn "," contents)

asciiValue :: Char -> Integer
asciiValue c = fromIntegral (ord c - ord 'A' + 1)

wordValue :: String -> Integer
wordValue = sum . map asciiValue

main = do
  withFile "problem_pages/project/resources/p042_words.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    let words = sort $ readWords contents
    let values = map wordValue words
    let triangleWords = filter (isTriangular . snd) (zip words values)
    print triangleWords
    print $ length triangleWords
    )