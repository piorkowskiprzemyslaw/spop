test (1:2:xs) = 1 + 2
test (1:xs) = 1
test _ = 0

data List a = None | Cons a (List a) deriving Show
data Tree a = Empty | Node a (List (Tree a)) deriving Show

t2 :: Tree Int
t2 = Node 4 (Cons (Node 1 None) (Cons (Node 2 None) None))

t3 :: Tree Int
t3 = Node 8 (Cons (Node 7 None) (Cons (Node 10 None) None))

t1 :: Tree Int
t1 = Node 5 (Cons t2 (Cons t3 None))

lmax :: (List a) -> (a -> Int) -> Int
lmax (Cons elem None) f = f elem
lmax (Cons elem rest) f = max (f elem) (lmax rest f)

depth :: (Tree Int) -> Int
depth Empty = 0
depth (Node _ None) = 1
depth (Node _ list) = 1 + (lmax list depth)