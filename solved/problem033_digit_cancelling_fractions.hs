-- 
-- Problem 33: Digit cancelling fractions
-- (Published on Friday, 20th December 2002, 06:00 pm; Solved by 39355)
-- 
--     The fraction ^{49}/_{98} is a curious fraction, as an
--  inexperienced mathematician in attempting to simplify it may
--  incorrectly believe that ^{49}/_{98} = ^{4}/_{8}, which is correct,
--  is obtained by cancelling the 9s.
-- 
--     We shall consider fractions like, ^{30}/_{50} = ^{3}/_{5}, to be
--  trivial examples.
-- 
--     There are exactly four non-trivial examples of this type of
--  fraction, less than one in value, and containing two digits in the
--  numerator and denominator.
-- 
--     If the product of these four fractions is given in its lowest
--  common terms, find the value of the denominator.

import Data.List
import Primes
--import Divisors

data Fraction = Fraction (Integer, Integer)
    deriving (Show)

normalise :: Fraction -> Fraction
normalise (Fraction (a, b)) =
    let d = gcd a b
    in Fraction (a `div` d, b `div` d)

instance Num Fraction where
    (*) (Fraction (a, b)) (Fraction (c, d)) = Fraction (a*c, b*d)
    (+) (Fraction (a, b)) (Fraction (c, d)) = Fraction (a*d + b*c, b*d)
    abs (Fraction (a, b)) = Fraction (abs a, abs b)
    signum (Fraction (a, b)) = Fraction (signum a * b, 1)
    fromInteger i = Fraction (i, 1)
    negate (Fraction (a, b)) = Fraction (a, -b)

instance Eq Fraction where
    (==) (Fraction (a, b)) (Fraction (c, d)) = fromInteger a / fromInteger b == fromInteger c / fromInteger d

isKDigitFraction k (Fraction (n, d)) = all (\x -> 10^(k-1) - 1 < x && x < 10^k) [n, d]

reducibleForms :: Fraction -> [Fraction]
reducibleForms (Fraction (n, d)) = map (\x -> Fraction (n*x, d*x)) [2..]

twoDigitReducibleForms :: Fraction -> [Fraction]
twoDigitReducibleForms f =
    let fracs = reducibleForms f
        below3 = takeWhile (not . isKDigitFraction 3) fracs
    in filter (isKDigitFraction 2) below3

irreducibleProperFractions :: [Fraction]
irreducibleProperFractions = [Fraction (n, d) | d <- [1..], n <- takeWhile (<d) (coprimes d)]

properFractions :: [Fraction]
properFractions = [Fraction (n, d) | d <- [1..], n <- [1..d-1]]

numDigits :: Integer -> Integer
numDigits n = 1 + fromIntegral (length (takeWhile ((<=n) . (10^)) [1..])) :: Integer

digitCancellingFractions :: [Fraction]
digitCancellingFractions =
    let fracs = takeWhile (isKDigitFraction 1) properFractions
        appendRight k = (+k) . (*10)
        appendLeft k n = n + k * 10 ^ numDigits n
        appendBoth k n = [appendLeft k n, appendRight k n]
        variations (Fraction (n, d)) k = [Fraction (a, b) | a <- appendBoth k n, b <- appendBoth k d]
        allVariations f = map (variations f) [1..9]
        digitCancellingVariations f = concatMap (filter (==f)) (allVariations f)
    in concatMap digitCancellingVariations fracs

main = do
    let l = digitCancellingFractions
        f = normalise $ product digitCancellingFractions
        (Fraction (_, d)) = f
    print digitCancellingFractions
    print f
    print d
