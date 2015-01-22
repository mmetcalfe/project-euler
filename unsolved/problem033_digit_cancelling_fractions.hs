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

type Fraction = (Integer, Integer)

isKDigitFraction k (n, d) = all (\x -> 10^(k-1) - 1 < x && x < 10^k) [n, d]

reducibleForms :: Fraction -> [Fraction]
reducibleForms (n, d) = map (\x -> (n*x, d*x)) [2..]

twoDigitReducibleForms :: Fraction -> [Fraction]
twoDigitReducibleForms f =
    let fracs = reducibleForms f
        below3 = takeWhile (not . (isKDigitFraction 3)) fracs
    in filter (isKDigitFraction 2) below3

irreducibleProperFractions :: [Fraction]
irreducibleProperFractions = [(n, d) | d <- [1..], n <- takeWhile (<d) (coprimes d)]

properFractions :: [Fraction]
properFractions = [(n, d) | d <- [1..], n <- [1..d-1]]

digitCancellingFractions :: [Fraction]
digitCancellingFractions =
    let fracs = takeWhile (isKDigitFraction 1) properFractions
        appendRight k = (+k) . (*10)
        appendLeft k n = n + k * 10 ^ numDigits n
    in fracs

main = do
    print $ digitCancellingFractions
