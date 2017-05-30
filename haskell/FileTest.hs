import Data.Ord
import Data.List
import Data.Monoid

import qualified Data.Map as Map

import Control.Arrow ((>>>))
import Control.Monad

import System.Environment
import System.Exit

countWords :: Int -> String -> [(String, Int)]
countWords n = words
  >>> fmap (\w -> (w, 1))
  >>> Map.fromListWith (+)
  >>> Map.toList
  >>> sortOn (Down . snd)
  >>> take n

main :: IO ()
main = do
  args <- getArgs

  (fileName, n) <- case args of
    [fileName, n] -> return (fileName, read n)
    [fileName]    -> return (fileName, 7)
    _             -> putStrLn "Wrong Arguments" >> exitFailure

  contents <- readFile fileName
  let pairs = countWords n contents

  putStrLn "Word Frequencies\n"

  forM_ pairs $ \(w, c) ->
    putStrLn $ w <> " : " <> show c

  putStrLn ""
