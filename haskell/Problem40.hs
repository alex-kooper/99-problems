import Data.Maybe
import Data.List

isPrime :: Int -> Bool
isPrime n = all isNotDivider [2..(isqrt n)]
  where
    isNotDivider d = n `rem` d /= 0
    isqrt = floor . sqrt . (fromIntegral :: Int -> Double)

goldbach :: Int -> (Int, Int)
goldbach n = (goldbachFirst, n - goldbachFirst)
  where
    isGoldbach n' = isPrime n' && isPrime (n - n')
    goldbachFirst = fromJust $ find isGoldbach [1..n `div` 2]

main :: IO ()
main = do
  putStrLn "Problem 40"

  putStr "Test 40: "
  print $ goldbach 28 == (5, 23)
