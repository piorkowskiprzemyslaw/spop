adjpairs :: [a] -> [(a,a)]
adjpairs []       = []
adjpairs [x]      = []
adjpairs (x:y:ss) = (x,y) : adjpairs (y:ss)