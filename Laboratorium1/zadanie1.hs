middle (x:y:_) [] = (x+y) / 2
middle (x:_) [_] = x
middle (_:xs) (_:_:ys) = middle xs ys

mid xs = middle xs xs