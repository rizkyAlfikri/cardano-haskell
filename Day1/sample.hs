-- Task 1
x = 4
y = x + 3

resultTask1 = x + x
-- resultTask1 = 8

-- Task 2
kurangi :: Int -> Int -> Int
kurangi g h = g - h

resultTask2 = kurangi 5 3
-- resultTask1 = 2

-- Task 3
multification :: Int -> Int -> Int
multification x y = x * y

resultTask3 = multification 4 5
-- resultTask3 20

-- Task 4
bagidua :: Fractional a => a -> a
bagidua x = x / 2

resultTask4 = bagidua 4
-- resultTask4 = 2.0

-- Task 5
myNestedCond :: Int -> Int -> Int -> Int
myNestedCond a b c
  | a <= b = a + 2
  | a <= c = a
  | otherwise = a - 2

resultTask5 = myNestedCond 10 2 5
-- resultTask5 = 8

-- Task 6
myNestedCondString :: Int -> String 
myNestedCondString a
  | a <= 2 = "Lebih kecil dari 2"
  | a <= 6 = "Lebih kecil dari 6"
  | otherwise = "Lebih besar dari 6"

resultTask6 = myNestedCondString 10
-- resultTask6 = "Lebih besar dari 6"