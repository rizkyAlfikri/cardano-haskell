-- Pattern Maching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- Where
initials:: String  -> String  -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where 
        (f:_) = firstName
        (l:_) = lastName


-- Guard with Where
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- Let it be
cyclinder:: (RealFloat  a) => a -> a -> a
cyclinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

-- Case expression
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

fun' :: (Int -> String) -> Int -> String
fun' x = x

-- Fibonanci
fibonanci' :: Int -> Int -> Int -> [Int]-> [Int]
fibonanci' _ _ 0 xs = reverse xs
fibonanci' 0 1 1 xs = [0]
fibonanci' 0 1 2 xs = [0,1]
fibonanci' 0 1 z xs = fibonanci' 1 1 (z - 3) [1, 1, 0]
fibonanci' x y z xs = fibonanci' y (x+y) (z-1) ((x+y):xs)

doFibonanci:: Int -> [Int]
doFibonanci x = fibonanci' 0 1 x []


-- Quick Sort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lesser ++ [x] ++ qsort greater
    where
        lesser = filter (< x) xs
        greater = filter (>= x) xs

--  Task 1
prependToList :: [a] -> a -> [a]
prependToList xs x = x:xs

resultTask1 = prependToList "askell" 'H'
-- Output : "Haskell"

--  Task 2
appendToList :: [a] -> [a] -> [a]
appendToList xs x = xs ++ x

resultTask2 = appendToList "Haskel" "l"
-- Output : "Haskell"

-- Task 3
headOfList :: [a] -> a
headOfList = head

resultTask3 = headOfList "Haskell"
-- Output : 'H'

-- Task 4
takeEleInList :: [a] -> [a]
takeEleInList xs
    | length xs > 10 = takedList
    | otherwise = xs
    where takedList = take 5 xs

resultTask4Ver1 = takeEleInList "i Love haskell"
resultTask4Ver2 = takeEleInList "Haskell"
-- Output Ver 1: 'i Lov'
-- Output Ver 2: 'Haskell'

-- Task 5
takeEleInListUpgraded :: [a] -> [a]
takeEleInListUpgraded [] = []
takeEleInListUpgraded xs = takeEleInList xs 

resultTask5Ver1 = takeEleInList "i Love haskell"
resultTask5Ver2 = takeEleInList "Haskell"
resultTask5Ver3 = takeEleInList ""
-- Output Ver 1: "i Lov"
-- Output Ver 2: "Haskell"
-- Output Ver 3: ""