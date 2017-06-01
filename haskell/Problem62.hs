
module Problem62 where

import Data.Monoid
import Tree

-- | Problem 62
--
-- >>> internals tree4
-- [1,2]
internals :: Tree a -> [a]
internals t = internals' t []
  where
    internals' Empty xs = xs
    internals' (Branch _ Empty Empty) xs = xs
    internals' (Branch x l r) xs = x : internals' l (internals' r xs)


-- | Problem 62A
--
-- >>> atLevel tree4 2
-- [2,2]
atLevel :: Tree a -> Int -> [a]
atLevel Empty _  = []
atLevel (Branch x l r) level
      | level <= 1 = [x]
      | otherwise  = atLevel l (level - 1) <> atLevel r (level - 1)
