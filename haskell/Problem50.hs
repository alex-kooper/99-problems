import Data.List
import qualified Data.IntMap as M
import Data.Monoid
import Control.Monad.State

type Frequency = (Char, Int)
type HuffmanCode = (Char, String)

-- Priority queue keeping lists of Huffman codes
type Queue = M.IntMap [HuffmanCode]

getMinFromQueue :: State Queue (Int, [HuffmanCode])
getMinFromQueue = state M.deleteFindMin

putIntoQueue :: Int -> [HuffmanCode] -> State Queue ()
putIntoQueue priority codes = state put'
  where put' q = ((), M.insert priority codes q)

step :: Queue -> Queue
step = execState go
  where
    go = do
      (p1, codes1) <- getMinFromQueue
      (p2, codes2) <- getMinFromQueue
      let codes1' = fmap (addDigit '0') codes1
      let codes2' = fmap (addDigit '1') codes2
      putIntoQueue (p1 + p2) (codes1' <> codes2')

    addDigit digit (ch, code) = (ch, digit:code)

huffman :: [Frequency] -> [HuffmanCode]
huffman xs =
  let initQueue = M.fromList . fmap toQueueElement $ xs
      resultQueue = iterate step initQueue !! (length xs - 1)
  in  sort . snd . head . M.toList $ resultQueue
  where
    toQueueElement (c, p) = (p, [(c, "")])

main :: IO ()
main = do
  putStrLn "Problem 50"

  putStr "Test 50: "
  print $ huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)] ==
    [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

