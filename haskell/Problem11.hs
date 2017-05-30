import Data.List

data Encoded a = Single a | Multiple Int a deriving Show

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = fmap encodeEach . group
    where encodeEach [x]    = Single x
          encodeEach (x:xs) = Multiple (length (x:xs)) x

test = encodeModified "aaaabccaadeeee"
