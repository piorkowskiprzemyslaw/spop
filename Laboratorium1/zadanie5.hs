import Data.Char

string2int :: String -> Int
string2int [x] = digitToInt x
string2int (x:xs) | isDigit x = customSum (1) (x:xs) (length (x:xs))
                  | otherwise = customSum (-1) xs (length xs)
                  	where customSum _ [] _     = 0
                  	      customSum s (x:xs) n = (10^n * s * (digitToInt x)) + (customSum s xs (n-1))
                  	      length [] = -1
                  	      length (x:xs) = 1 + length xs 