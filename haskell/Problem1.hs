
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' = foldr1 (\x y -> y)
