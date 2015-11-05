import Data.Char

codeCezar :: String -> Int -> String
decodeCezar :: String -> Int -> String

codeCezar [] _     = []
codeCezar (x:xs) s = (chr ((ord x) + s)) : codeCezar xs s

decodeCezar [] _     = []
decodeCezar (x:xs) s = (chr ((ord x) -s)) : decodeCezar xs s