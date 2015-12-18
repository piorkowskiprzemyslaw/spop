------ GRUPA B
--zadanie 1
fun :: Eq a => (t -> a) -> t -> [t] -> Bool
fun w x [] = False
fun w x (y:z) | w x == w y = True
              | otherwise = fun w x z

--zadanie 2
data Nat = Zero | Succ Nat deriving (Show, Eq)

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n-1))

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

add :: Nat -> Nat -> Nat
add n1 Zero = n1
add n1 (Succ n2) = Succ (add n1 n2)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult n1 (Succ Zero) = n1
mult n1 (Succ n2) = add n1 (mult n1 n2)

--zadanie 3
data List a = Empty | Cons a (List a)

l :: List Int
l = Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))

l1 :: List Int
l1 = Cons 1 (Cons 2 (Cons 3 (Cons 3 Empty)))

l2 :: List Int
l2 = Cons 2 (Cons 2 (Cons 2 (Cons 2 Empty)))

checkNext :: Ord a => List a -> (a -> Bool) -> Bool
checkNext Empty p = True
checkNext (Cons e _) p = p e

sorted :: Ord a => List a -> Bool
sorted Empty = True
sorted (Cons e tail) = (checkNext tail (\x->x > e)) && sorted tail

------------ GRUPA A
-- zadanie 1

checkAll :: Eq a => List a -> (a -> Bool) -> Bool
checkAll Empty p = True;
checkAll (Cons e tail) p = (p e) && (checkAll tail p)

allEqual :: Eq a => List a -> Bool
allEqual Empty = True;
allEqual (Cons e tail) = checkAll tail (\x -> x == e)

-- zadanie 2
fun1 :: Ord a => (t -> a) -> [t] -> t

fun1 w [x] = x
fun1 w (x:y:z)   | w x > w y = fun1 w (x:z)
                 | otherwise = fun1 w (y:z)

-- zadanie 3

sub :: Nat -> Nat -> Nat
sub (Succ n1) n2    | (Succ n1) == n2 = Zero
                    | otherwise = Succ (sub n1 n2)