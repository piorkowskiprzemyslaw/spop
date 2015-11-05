insertAt :: [a] -> a -> Int -> [a]

insertAt xs e 0     = (e:xs)
insertAt [] e _     = error "index out of bound!"
insertAt (x:xs) e n = x : insertAt xs e (n-1)