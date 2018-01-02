-- numberInWords.hs

module NumberInWords where

myUnits :: [(String, Integer)]
myUnits = [("billion ", 10^9),
           ("million ", 10^6),
           ("thousand ", 10^3),
           ("hundred ", 100),
           ("ninety ", 90),
           ("eighty ", 80),
           ("seventy ", 70),
           ("sixty ", 60),
           ("fifty ", 50),
           ("forty ", 40),
           ("thirty ", 30),
           ("twenty ", 20),
           ("nineteen ", 19),
           ("eighteen ", 18),
           ("seventeen ", 17),
           ("sixteen ", 16),
           ("fifteen ", 15),
           ("fourteen ", 14),
           ("thirteen ", 13),
           ("twelve ", 12),
           ("eleven ", 11),
           ("ten ", 10),
           ("nine ", 9),
           ("eight ", 8),
           ("seven ", 7),
           ("six ", 6),
           ("five ", 5),
           ("four ", 4),
           ("three ", 3),
           ("two ", 2),
           ("one ", 1),
           ("zero ", 0)]

num2words :: Integer -> [(String, Integer)] -> Integer -> String
num2words 0 _ _ = ""
num2words 1 _ _ = "one "
num2words n units barrier = let (word, divisor) = getGreatestDivisor n units
                                quotient = n `div` divisor
                                remainder = n `mod` divisor
                                in if divisor < barrier
                                      then word ++ num2words remainder (tail units) barrier
                                      else num2words quotient units barrier ++  word ++  num2words remainder (tail units) barrier

getGreatestDivisor :: Integer -> [(String, Integer)] -> (String, Integer)
getGreatestDivisor n ((word, val): xs) = if n >= val
                                            then (word, val)
                                            else getGreatestDivisor n xs

answer :: Integer -> String
answer n = init (num2words n myUnits 100)


{-
answer 10023
"ten thousand twenty three"

answer 100
"one hundred"

answer 123456789456
"one hundred twenty three billion four hundred fifty six million seven hundred eighty nine thousand four hundred fifty six"

answer 785463214789
"seven hundred eighty five billion four hundred sixty three million two hundred fourteen thousand seven hundred eighty nine"

answer 2
"two"

answer 25
"twenty five"

answer 1125
"one thousand one hundred twenty five"

answer 12
"twelve"

answer 392
"three hundred ninety two"

answer 9234112
"nine million two hundred thirty four thousand one hundred twelve"
-}
