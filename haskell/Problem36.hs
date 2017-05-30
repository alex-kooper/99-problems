import Data.Maybe
import Data.List

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = minFactor : primeFactors (n `quot` minFactor)
  where
    minFactor = fromJust $ find (\f -> n `rem` f == 0) [2..n]

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = fmap encode . group . primeFactors
  where
    encode xs = (head xs, length xs)

main :: IO ()
main = do
  putStrLn "Problem 36"

  putStr "Test 1: "
  print $ primeFactorsMult 315 == [(3, 2), (5, 1), (7, 1)]
