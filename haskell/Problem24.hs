import Data.List
import Data.Monoid
import System.Random

diffSelect :: Int -> Int -> IO [Int]
diffSelect n maxN = draw n [1..maxN]
  where
    draw _ [] = return []
    draw 0 _ = return []
    draw n' unselected = do
      randomN <- randomRIO (0, length unselected - 1)
      let (e, unselected') = removeOne randomN unselected
      selected <- draw (n' - 1) unselected'
      return $ e : selected

    removeOne i xs =
      let (before, rest) = splitAt i xs
      in  (head rest, before <> tail rest)


diffSelect' :: Int -> Int -> IO [Int]
diffSelect' n maxN = drawN <$> getStdGen
  where
    drawN = take n . nub . randomRs (1, maxN)

main :: IO ()
main = do
  putStrLn "Problem 24"

  putStr "Test 1: "
  show <$> diffSelect 6 49 >>= putStrLn

  putStr "Test 2: "
  show <$> diffSelect' 6 49 >>= putStrLn
