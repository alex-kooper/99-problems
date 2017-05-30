
compress :: (Eq a) => [a] -> [a]
compress = foldr add []
    where add x [] = [x]
          add x (y:ys)
              | x == y    = y:ys
              | otherwise = x:y:ys
