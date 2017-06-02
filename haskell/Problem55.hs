module Problem55 where

import Data.Monoid

import Tree

-- | Problem 62A
--
-- >>> cbalTree 4 == resultTrees
-- True
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]

cbalTree n =
  let n1 = (n - 1) `div` 2
      n2 = n - n1 - 1
  in  if n1 == n2
        then [Branch 'x' x y | x <- cbalTree n1, y <- cbalTree n2]
        else [Branch 'x' x y | x <- cbalTree n1, y <- cbalTree n2] <>
             [Branch 'x' x y | x <- cbalTree n2, y <- cbalTree n1]

resultTrees :: [Tree Char]
resultTrees =
  [
  -- permutation 1
  --     x
  --    / \
  --   x   x
  --        \
  --         x
  Branch 'x' (Branch 'x' Empty Empty)
             (Branch 'x' Empty
                         (Branch 'x' Empty Empty)),

  -- permutation 2
  --     x
  --    / \
  --   x   x
  --      /
  --     x
  Branch 'x' (Branch 'x' Empty Empty)
             (Branch 'x' (Branch 'x' Empty Empty)
                         Empty),

  -- permutation 3
  --     x
  --    / \
  --   x   x
  --    \
  --     x
  Branch 'x' (Branch 'x' Empty
                         (Branch 'x' Empty Empty))
             (Branch 'x' Empty Empty),

  -- permutation 4
  --     x
  --    / \
  --   x   x
  --  /
  -- x
  Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
                         Empty)
             (Branch 'x' Empty Empty)
  ]
