module Problem18 where

-- | Problem 18
--
-- >>> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
--
slice :: [a] -> Int -> Int -> [a]
slice xs from to =  fmap snd . filter isInRange $ zip [1..] xs
  where isInRange (n, _) = n >= from && n <= to

main :: IO ()
main = do
  putStrLn "Problem 18"

  putStr "Test 1: "
  print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg"
