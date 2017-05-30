import Data.List
import Data.Monoid
import Control.Monad

import Problem47

tablen' :: Int -> ([Bool] -> Bool) -> String
tablen' n f = unlines $ fmap line boolProduct
  where
    line xs = intercalate "\t" (show <$> xs) <> "\t" <> show (f xs)
    boolProduct = replicateM n [True, False]

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = putStrLn $ tablen' n f

main :: IO ()
main = do
  putStrLn "Problem 48\n"

  putStrLn "Output\n"
  tablen 3 (\[a, b, c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
  putStrLn ""

  putStr "Test 1: "
  print $ tablen' 3 (\[a, b, c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c) ==
    "True\tTrue\tTrue\tTrue\n\
    \True\tTrue\tFalse\tTrue\n\
    \True\tFalse\tTrue\tTrue\n\
    \True\tFalse\tFalse\tTrue\n\
    \False\tTrue\tTrue\tTrue\n\
    \False\tTrue\tFalse\tTrue\n\
    \False\tFalse\tTrue\tTrue\n\
    \False\tFalse\tFalse\tTrue\n"
