
split :: [a] -> Int -> ([a], [a])
split xs n = (prefix, suffix)
  where
    (prefix, suffix, _) = foldr step ([], [], 0) xs
    tailLength = length xs - n

    step v (p, s, count)
      | count < tailLength = (p, v:s, count + 1)
      | otherwise = (v:p, s, count + 1)

split' :: [a] -> Int -> ([a], [a])
split' xs n = (take n xs, drop n xs)

main :: IO ()
main = do
  putStrLn "Problem 17"

  putStr "Test 1: "
  print $ split "abcdefghik" 3 == ("abc", "defghik")

  putStr "Test 2: "
  print $ split' "abcdefghik" 3 == ("abc", "defghik")
