cc :: [[a]] -> [a]
cc []    = []
cc (xs:xss) = xs ++ cc xss