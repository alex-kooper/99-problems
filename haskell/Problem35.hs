import Data.Maybe
import Data.List

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = minFactor : primeFactors (n `quot` minFactor)
  where
    minFactor = fromJust $ find (\f -> n `rem` f == 0) [2..n]

main :: IO ()
main = do
  putStrLn "Problem 35"

  putStr "Test 1: "
  print $ primeFactors 315 == [3, 3, 5, 7]
