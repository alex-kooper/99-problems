
coprime :: (Integral a) => a -> a -> Bool
coprime a b = gcd a b == 1

main :: IO ()
main = do
  putStrLn "Problem 33"

  putStr "Test 1: "
  print $ coprime (35 :: Int) (64 :: Int)
