
elementAt :: [a] -> Int -> a
elementAt xs n = head $ drop (n - 1) xs
