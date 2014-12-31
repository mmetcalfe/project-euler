-- 
-- Problem 19: Counting Sundays
-- (Published on Friday, 14th June 2002, 06:00 pm; Solved by 72361)
-- 
--     You are given the following information, but you may prefer to
-- 	do some research for yourself.
-- 
--         - 1 Jan 1900 was a Monday.
--         - Thirty days has September,
--           April, June and November.
--           All the rest have thirty-one,
--           Saving February alone,
--           Which has twenty-eight, rain or shine.
--           And on leap years, twenty-nine.
--     - A leap year occurs on any year evenly divisible by 4, but not
-- 	on a century unless it is divisible by 400.
-- 
-- 
--     How many Sundays fell on the first of the month during the
-- 	twentieth century (1 Jan 1901 to 31 Dec 2000)?

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

incrementDay :: Weekday -> Weekday
incrementDay Monday    = Tuesday
incrementDay Tuesday   = Wednesday
incrementDay Wednesday = Thursday
incrementDay Thursday  = Friday
incrementDay Friday    = Saturday
incrementDay Saturday  = Sunday
incrementDay Sunday    = Monday

incrementMonth :: Month -> Month
-- incrementMonth m = (m + 1) `mod` 12
incrementMonth Jan = Feb
incrementMonth Feb = Mar
incrementMonth Mar = Apr
incrementMonth Apr = May
incrementMonth May = Jun
incrementMonth Jun = Jul
incrementMonth Jul = Aug
incrementMonth Aug = Sep
incrementMonth Sep = Oct
incrementMonth Oct = Nov
incrementMonth Nov = Dec
incrementMonth Dec = Jan

monthOfYear Jan = 0
monthOfYear Feb = 1
monthOfYear Mar = 2
monthOfYear Apr = 3
monthOfYear May = 4
monthOfYear Jun = 5
monthOfYear Jul = 6
monthOfYear Aug = 7
monthOfYear Sep = 8
monthOfYear Oct = 9
monthOfYear Nov = 10
monthOfYear Dec = 11

type Year = Integer
-- type Month = Integer
type Day = Integer

data Date = Date Year Month Day Weekday deriving (Show)

isLeapYear :: Year -> Bool
isLeapYear y =
    if y `mod` 100 == 0
        then y `mod` 400 == 0
        else y `mod` 4 == 0

-- J 0
-- F 1
-- M 2
-- A 3
-- M 4
-- J 5  *
-- J 6
-- A 7  *
-- S 8  *
-- O 9
-- N 10 *
-- D 11

daysInMonth :: Year -> Month -> Integer
daysInMonth _ Sep = 30
daysInMonth _ Apr = 30
daysInMonth _ Jun = 30
daysInMonth _ Nov = 30
daysInMonth y Feb
    | isLeapYear y = 29
    | otherwise = 28
daysInMonth _ _ = 31


incrementDate :: Date -> Date
incrementDate (Date y m d wd) = Date y' m' d' wd'
    where wd' = incrementDay wd
          d' = (d + 1) `mod` daysInMonth y m
          monthEnd = d' == 0
          m' = if monthEnd then incrementMonth m else m
          yearEnd = monthEnd && m' == Jan
          y' = if yearEnd then y+1 else y

daysInYear :: Year -> Integer
daysInYear y = sum (map (daysInMonth y) [Jan .. Dec])

daysInCentury :: Year -> Integer
daysInCentury y = sum (map daysInYear [y..y+99])

isSundayOnFirst :: Date -> Bool
isSundayOnFirst (Date _ _ d wd) = d == 0 && wd == Sunday

daysUntil :: Year -> Integer
daysUntil y = sum (map daysInYear [1900..y-1])

datesFrom1900 :: [Date]
datesFrom1900 = iterate incrementDate (Date 1900 Jan 0 Monday)

datesInCentury :: Year -> [Date]
datesInCentury y =
    let dropDays = daysUntil y
        takeDays = daysInCentury y
    in (take (fromIntegral takeDays) . drop (fromIntegral dropDays)) datesFrom1900


-- year, month, day
-- increment


main :: IO ()
main = do
--     let dates = iterate incrementDate (Date 1900 0 0 Monday)
--     let century = take (fromIntegral $ daysInCentury 1901) dates
--     let century = take (fromIntegral $ daysUntil 2014) dates
    let century = datesInCentury 1901
        firstSundays = filter isSundayOnFirst century
    print $ daysUntil 1901
    print $ daysInCentury 1901
    print $ century
    print $ length firstSundays
