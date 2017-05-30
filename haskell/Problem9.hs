
pack :: (Eq a) => [a] -> [[a]]
pack = foldr add []
    where add e [] = [[e]]
          add e ((x:xs):ys)
              | e == x    = (e:x:xs):ys
              | otherwise = [e]:(x:xs):ys

test = pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
