
myLength :: [a] -> Int
myLength = sum . fmap (const 1)
