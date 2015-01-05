--
-- Problem 22: Names scores
-- (Published on Friday, 19th July 2002, 06:00 pm; Solved by 72519)
--
--     Using [names.txt](project/resources/p022_names.txt) (right click
-- 	and 'Save Link/Target As...'), a 46K text file containing over
-- 	five-thousand first names, begin by sorting it into alphabetical
-- 	order. Then working out the alphabetical value for each name,
-- 	multiply this value by its alphabetical position in the list to
-- 	obtain a name score.
--
--     For example, when the list is sorted into alphabetical order,
-- 	COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name
-- 	in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
--
--     What is the total of all the name scores in the file?

import System.IO
import qualified Data.List.Split as Split
import Data.List
import Data.Char

readNames :: String -> [String]
readNames contents = map (filter (/='"')) (Split.splitOn "," contents)

alphabeticalValue :: String -> Int
alphabeticalValue = sum . map (\c -> ord c - ord 'A' + 1)

main = do
  withFile "problem_pages/project/resources/p022_names.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    let names = sort $ readNames contents
    let values = map alphabeticalValue names
    let scores = zipWith (*) values [1..]
    print $ scores
    print $ sum scores
    )
