module Tree where

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
