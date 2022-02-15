-- Task 1
skips:: [a] -> [[a]]
skips [] = [[]]
skips xs = init $ skipsHelper xs xs 1

skipsHelper  :: [a] -> [a]  -> Int -> [[a]]
skipsHelper _ [] _ = [[]]
skipsHelper _ _ 0 = [[]]
skipsHelper xs ys i = takeList xs [] i i : skipsHelper xs (tail ys) (i+1)

takeList :: [a] -> [a] -> Int  -> Int -> [a]
takeList [] ys _ _ = reverse ys
takeList xs ys 1 j  = takeList (tail xs) (head xs : ys) j j
takeList xs ys i j= takeList (tail xs) ys (i - 1) j


-- Task 2
localMaxima:: (Integral a) => [a] -> [a]
localMaxima [] = []
localMaxima xs
    | isSizeListValid = calculatedResult
    | otherwise = []
    where isSizeListValid =  3 <=  length xs
          isElementValid = (xs !! 1) > head xs && (xs !! 1) > (xs !! 2)
          calculatedResult = if isElementValid then xs !! 1: localMaxima (tail xs)  else localMaxima $ tail xs

-- Test Result Task 1
outputSkips1 = skips "ABCD"
outputSkips2 = skips "hello!"
outputSkips3 = skips [1]
outputSkips4 = skips [True, False]
outputSkips5 = skips []

-- Test Result Task 2
outputLocalMaxima1 = localMaxima [2,9,5,6,1]
outputLocalMaxima2 = localMaxima [2,3,4,1,5]
outputLocalMaxima3 = localMaxima [1,2,3,4,5]