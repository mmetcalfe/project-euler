module Figurates where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

choose :: Integer -> Integer -> Integer
choose n k = factorial n `div` (factorial k * factorial (n - k))

polytopic :: Integer -> Integer -> Integer
polytopic r n = (n + r - 1) `choose` r

tetrahedral :: Integer -> Integer
tetrahedral n = (n*(n+1)*(n+2)) `div` 6

triangular :: Integer -> Integer
triangular n = (n*(n+1)) `div` 2

square :: Integer -> Integer
square n = n^2

pentagonal :: Integer -> Integer
pentagonal n = (n * (3*n - 1)) `div` 2

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)

triangularIndex :: Integer -> Integer
triangularIndex n = (floor . sqrt . fromIntegral . (*2)) n
isTriangular :: Integer -> Bool
isTriangular n = (triangular . triangularIndex) n == n

pentagonalIndex :: Integer -> Integer
pentagonalIndex n = ((+1) . floor . sqrt . (/3) . fromIntegral . (*2)) n
isPentagonal :: Integer -> Bool
isPentagonal n = (pentagonal . pentagonalIndex) n == n

hexagonalIndex :: Integer -> Integer
hexagonalIndex n = ((+1) . floor . sqrt . (/2) . fromIntegral) n
isHexagonal :: Integer -> Bool
isHexagonal n = (hexagonal . hexagonalIndex) n == n
