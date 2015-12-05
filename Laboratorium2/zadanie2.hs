--zadanie 2
mymap f xs = [(f x) | x <- xs]

myfilter f xs = [x | x <- xs, (f x)]