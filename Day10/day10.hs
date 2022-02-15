fun1 :: [Int] -> Int 
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Int -> Int
fun2 1 = 0
fun2 n | even n = n + fun2(n `div` 2)
       | otherwise = fun2(3 * n + 1)

inputListTupple2 :: [(Char, Char)]
inputListTupple2 = [(x, y) | x <- "a", y <- "b"]


inputListTupple3 :: [(Char, Int)]
inputListTupple3 = [(x, y) | x <- "a", y <- [2]]

inputListTupple4 :: Char -> Char -> [(Char, Char)]
inputListTupple4 x y = [(x, y)]

zipOutput = zip [1] [2]

takeUntil' :: (a -> Bool) -> [a] -> [a]
takeUntil' p = foldl (\acc x -> ) []