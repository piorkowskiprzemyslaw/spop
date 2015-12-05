-- zadanie 5

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

t :: Tree Int
t = Node 5 (Node 3 (Node 8 Empty Empty)
                   (Node 1 Empty Empty))
           (Node 4 Empty
                   (Node 6 Empty Empty))

-- zwraca listę węzłów wewnętrznych drzewa (węzeł
-- wewnętrzny to taki, który posiada jeden lub dwa niepuste następniki)

isEmpty Empty  = True;
isEmpty _      = False;

interNodes :: Tree a -> [a]
interNodes Empty       = []
interNodes (Node a l r) | isEmpty l && isEmpty r = interNodes l ++ interNodes r
                        | otherwise              = interNodes l ++ [a] ++ interNodes r

-- zwraca odbicie zwierciadlane danego drzewa (lewe i prawe poddrzewa są zamienione)

mirrorTree :: Tree a -> Tree a
mirrorTree Empty = Empty
mirrorTree (Node a l r) = Node a (mirrorTree r) (mirrorTree l)

-- zwraca listę wierzchołków znajdujących się na podanej głębokości
-- w drzewie (korzeń drzewa jest na głębokości 1, jego następniki są na głębokości 2 itd.)

nodesAtLevel :: Tree a -> Int -> [a]
nodesAtLevel (Node a _ _ ) 1    = [a]
nodesAtLevel Empty 1            = []
nodesAtLevel (Node _ l r ) lvl  = (nodesAtLevel l (lvl - 1)) ++ (nodesAtLevel r (lvl - 1))