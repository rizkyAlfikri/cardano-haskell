-- Task1
-- A.   ['a','b','c'] -> [Char]
-- B.   ('a','b','c') -> [Char, Char , Char]
-- C.   [(False,'0'),(True,'1')] -> [(Bool, Char)]
-- D.   ([False, True],['0','1']) -> ([Bool], [Char])
-- E.   [tail, init, reverse] -> [[a] -> [a]]

-- Task 2
-- A.  second xs = head (tail xs)  => second :: [a] -> a
-- B.  swap (x,y) = (y,x) => swap :: (a, b) -> (b,a) 
-- C.  pair x y = (x,y) => pair :: a -> a -> (a,a)
-- D.  double x = x * 2 => double :: Num a => a -> a
-- E.  palindrome xs = reverse xs == xs => palindrome :: [[a] -> [a]]
-- F.  twice f x = f (f x) => twice :: (a -> a) -> a -> a