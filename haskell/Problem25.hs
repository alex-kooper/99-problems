import Data.List
import System.Random

rndPermu :: [a] -> IO [a]
rndPermu xs = randomPermutations <$> getStdGen
  where
    xsLength = length xs
    randomNumbers = take xsLength . nub . randomRs (0, xsLength - 1)
    randomPermutations = fmap (xs !!) . randomNumbers

main :: IO ()
main = do
  putStrLn "Problem 25"

  putStr "Test 1: "
  rndPermu "abcdef"  >>= putStrLn
