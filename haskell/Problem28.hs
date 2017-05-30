import Data.List
import Data.Map hiding (map)

lsort :: [[a]] -> [[a]]
lsort = sortOn length

lfsort :: [[a]] -> [[a]]
lfsort xs = sortOn lengthFreq xs
  where
    lengthFreqMap = fromListWith (+) $ fmap mkTuple xs
    mkTuple xs' = (length xs', 1 :: Int)
    lengthFreq = (lengthFreqMap !) . length

main :: IO ()
main = do
  putStrLn "Problem 28"

  let input = ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]

  putStr "Test 1: "
  let output1 = ["o", "de", "de", "mn", "abc", "fgh", "ijkl"]
  print $ lsort input == output1

  putStr "Test 2: "
  let output2 = ["ijkl", "o", "abc", "fgh", "de", "de", "mn"]
  print $ lfsort input == output2
