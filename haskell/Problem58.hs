
module Problem57 where

import Tree
import Problem55
import Problem56

-- | Problem 57
--
-- >>> symBalTrees 5 == trees58
-- True
symBalTrees :: Int -> [Tree Char]
symBalTrees = filter symmetric . cbalTree

trees58 :: [Tree Char]
trees58 =
  [ Branch 'x'
    (Branch 'x' Empty (Branch 'x' Empty Empty))
    (Branch 'x' (Branch 'x' Empty Empty) Empty)
  ,
    Branch 'x'
    (Branch 'x' (Branch 'x' Empty Empty) Empty)
    (Branch 'x' Empty (Branch 'x' Empty Empty))
  ]
