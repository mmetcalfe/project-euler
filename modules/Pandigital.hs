module Pandigital where

import Data.List

isPandigital :: String -> Bool
isPandigital n = length n == 9 && sort n == "123456789"

isNDigitPandigital :: String -> Bool
isNDigitPandigital n =
    let l = length n
    in (l <= 9) && (sort n == take l "123456789")
