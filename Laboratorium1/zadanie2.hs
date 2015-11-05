removeDups [x]      = [x]
removeDups (x:y:xs) = if x /= y then x : removeDups (y:xs)
                                else removeDups (y:xs)