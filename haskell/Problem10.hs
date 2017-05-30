import Data.List

encode = fmap encodeOne . group
    where encodeOne xs = (length xs, head xs)

test = encode "aaaabccaadeeee"
