
myGCD :: Int -> Int -> Int
myGCD a' b' = gcd' (abs a') (abs b')
  where
    gcd' a b
      | b == 0    = a
      | a > b     = gcd' b (a `mod` b)
      | a < b     = gcd' a (b `mod` a)
      | otherwise = a

main :: IO ()
main = do
  putStrLn "Problem 32"

  putStr "Test 1: "
  print $ [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] == [9, 3, 3]
