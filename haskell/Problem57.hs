
module Problem57 where

import Tree
import Problem56

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Branch x Empty Empty
insert x (Branch y l r)
  | x < y     = Branch y (insert x l) r
  | otherwise = Branch y l (insert x r)

-- | Problem 57
--
-- >>> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
--
-- >>> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
--
-- >>> symmetric . construct $ [3, 2, 5, 7, 1]
-- True
construct :: Ord a => [a] -> Tree a
construct = foldl (flip insert) Empty
