module Digits where

numDigits :: Integer -> Integer
numDigits n = 1 + fromIntegral (length (takeWhile ((<=n) . (10^)) [1..])) :: Integer
