-- Napisz rekurencyjną funkcję begin xs, ys, która zwraca True, jeżeli xs jest prefiksem ys.
-- Oba argumenty są listami elementów typu Int. Podaj typ funkcji begin

begin :: Eq a => [a] -> [a] -> Bool
begin [] _ = True
begin _ [] = False
begin (x:xs) (y:ys) | x == y = begin xs ys
                    | otherwise = False

-- Dla następującej definicji drzewa binarnego napisz funkcję Drzewo -> Maybe Int,
-- która dla drzewa pustego zwraca Nothing, a dla drzewa niepustegom, jego największy element

data Drzewo = Puste | Wierzch Int Drzewo Drzewo

maxElem :: Drzewo -> Maybe Int
maxElem Puste = Nothing
maxElem (Wierzch e l r) = max (Just e) (max (maxElem l) (maxElem r))

-- Napisz rekurencyjną funkcję removeAt xs n, która usuwa z listy xs element na pozycji n.
-- Gdy n jest większe niż długość listy funkcja powinna wygenerować wyjątek.
-- Argument n jest typu Int, a xs jest listą elementów typu Char.
-- Podaj typ funkcji removeAt

removeAt :: String -> Int -> String
removeAt [] _ = error "n is greater than length of string"
removeAt (x:xs) 1 = xs
removeAt (x:xs) n = x : removeAt xs (n - 1)


-- Dla następującej definicji drzewa binarnego:
data Tree = Node Int Tree Tree | Empty
-- napisz funkcję isHeap: Tree -> Bool, która dla podanego drzewa zwraca True jeżeli drzewo tworzy kopiec,
-- tj wartość rodzica jest zawsze większa lub równa wartości potomka.
-- W przeciwnym wypadku funkcja zwraca False.
-- Dla drzewo pustego funkcja zwraca True.

t :: Tree
t = Node 10 (Node 7 (Node 2 Empty Empty) (Node 9 Empty Empty))
            (Node 8 (Node 5 Empty Empty) (Node 4 Empty Empty))

checkPred :: Tree -> (Int -> Bool) -> Bool
checkPred Empty _ = True
checkPred (Node e _ _) p = p e

isHeap :: Tree -> Bool
isHeap Empty = True
isHeap (Node e l r) = let pred = (\x -> x < e) in
                        (checkPred l pred) && (checkPred r pred) && isHeap l && isHeap r