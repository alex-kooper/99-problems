module Problem47
  (and', or', nand', nor', xor', impl', equ')
where

import Text.Printf

infixl 6 `and'`
and' :: Bool -> Bool -> Bool
and' = (&&)

infixl 4 `or'`
or' :: Bool -> Bool -> Bool
or' = (||)

infixl 6 `nand'`
nand' :: Bool -> Bool -> Bool
nand' x y = not $ x && y

infixl 4 `nor'`
nor' :: Bool -> Bool -> Bool
nor' x y = not $ x || y

infixl 4 `xor'`
xor' :: Bool -> Bool -> Bool
xor' = (/=)

infixl 3 `impl'`
impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _    _     = True

infixl 3 `equ'`
equ' :: Bool -> Bool -> Bool
equ' = (==)

table' :: (Bool -> Bool -> Bool) -> String
table' f = unlines [line a b | a <- boolValues, b <- boolValues]
  where
    line a b = printf "%s %s %s" (show a) (show b) (show $ f a b)
    boolValues = [True, False]

table :: (Bool -> Bool -> Bool) -> IO ()
table = putStrLn . table'

main :: IO ()
main = do
  putStrLn "Problem 46\n"

  putStrLn "Output\n"
  table (\a b -> a `and'` (a `or'` not b))
  putStrLn ""

  putStr "Test 1: "
  print $ table' (\a b -> a `and'` (a `or'` not b)) == "True True True\n\
                                                       \True False True\n\
                                                       \False True False\n\
                                                       \False False False\n"
