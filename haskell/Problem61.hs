
module Problem61 where

import Data.Monoid
import Tree

-- | Problem 61
--
-- >>> countLeaves tree4
-- 2
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ lb rb) = countLeaves lb + countLeaves rb

-- | Problem 61A
--
-- >>> leaves tree4
-- [4,2]
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ lb rb) = leaves lb <> leaves rb
