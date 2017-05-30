import Data.Monoid

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = fmap (x :) (combinations (n - 1) xs) <> combinations n xs

main :: IO ()
main = do
  putStrLn "Problem 26"

  putStr "Test 1: "
  print $ length (combinations 3 "abcdef") == 20
