removeDups' [x] = [x]
removeDups' (x:xs) | not (contains xs x) = x : removeDups' xs
                   | otherwise           = removeDups' xs
                      where contains [] _     = False
                            contains (y:ys) x | x == y    = True
                                              | otherwise = contains ys x