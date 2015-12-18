
-- Na jednej są elementy spełniające dany predykat a na drugiej nie.
myFilter f xs = [x | x <- xs, f x]

split :: [a] -> (a -> Bool) -> ([a],[a])
split xs f = ((myFilter f xs), (myFilter (\x -> (not (f x))) xs))

splitInner [] _ t = t
splitInner (x:xs) f (as, bs) = case f x of
                                    True  -> splitInner xs f (x:as, bs)
                                    False -> splitInner xs f (as, x:bs)

split' xs f = splitInner xs f ([], [])

-- Napisz funkcję rozdzielająca daną listę na dwie różniące się długością co najwyżej o 1.

split2 :: [a] -> ([a],[a])

split2Inner [] t _ = t
split2Inner (x:xs) (as, bs) c = case c of
                                    True  -> split2Inner xs (x:as, bs) False
                                    False -> split2Inner xs (as, x:bs) True

split2 as = split2Inner as ([], []) True

-- Drzewo jest binarne gdy w lewym poddrzewie danego węzła są elementy nie większe niż dany element
-- a w prawym analogicznie.

data Tree = Empty | Node Int Tree Tree deriving Show

t :: Tree
t = Node 5 (Node 3 (Node 1 Empty Empty)
                   (Node 4 Empty Empty))
           (Node 8 (Node 7 Empty Empty)
                   (Node 10 Empty Empty))

checkPred :: Tree -> (Int -> Bool) -> Bool
checkPred Empty _ = True
checkPred (Node e l r) p = case p e of
                                True -> checkPred l p && checkPred r p
                                _ -> False

isBST :: Tree -> Bool
isBST Empty = True
isBST (Node e l r) = checkPred l (\x-> x<e) && checkPred r (\x-> x>e) && isBST l && isBST r

-- Za pomocą list comprahension podaj wszystkie trójki liczb nie większych
-- niż 10, które spełniają warunek x^2 + y^2 = z^2
fun = [(x,y,z) | x <- [1..10], y <- [x..10], z <- [1..10], x^2 + y^2 == z^2]
