
module Problem62 where

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
