-- zadanie 4
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum)

whichDay :: Day -> Int -> Day

whichDay day number = toEnum ( ((fromEnum day) + number - 1) `mod` 7 ) :: Day
