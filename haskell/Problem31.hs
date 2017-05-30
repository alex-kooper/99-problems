
isPrime :: Int -> Bool
isPrime n = all isNotDivider [2..(isqrt n)]
  where
    isNotDivider d = n `rem` d /= 0
    isqrt = floor . sqrt . (fromIntegral :: Int -> Double)

main :: IO ()
main = do
  putStrLn "Problem 31"

  putStr "Test 1: "
  print $ isPrime 7
