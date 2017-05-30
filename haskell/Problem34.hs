import Data.List

totient :: (Integral a) => a -> a
totient 1 = 1
totient n = genericLength $ filter ((== 1) . gcd n) [1..(n - 1)]


main :: IO ()
main = do
  putStrLn "Problem 34"

  putStr "Test 1: "
  print $ totient (10 :: Int) == 4
