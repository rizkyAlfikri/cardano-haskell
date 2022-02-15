import Text.Read (Lexeme(Char))
data IntList = Empty | Cons Int IntList deriving Show

--  Custom Map
addOneToInt :: IntList -> IntList
addOneToInt Empty = Empty
addOneToInt (Cons x xs) = Cons (x + 1) (addOneToInt xs)

myIntList = Cons 2 (Cons (-3) (Cons 5 Empty))

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons  (abs x)  (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons  (x * x)  (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

addOne = (+1)
square x = x * x

addOneToAll = mapIntList addOne
absToAll = mapIntList abs
squareToAll = mapIntList square

keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs)
    | x > 0 = Cons x $ keepOnlyPositive xs
    | otherwise = keepOnlyPositive xs

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x = Cons x $ keepOnlyEven xs
    | otherwise = keepOnlyEven xs

data List t = E | C t (List t) deriving Show

lst1 :: List Int
lst1 = C 3 (C (-1) (C 5 E))

lst2 :: List Char
lst2 = C 'A' (C 'V' (C 'T' E))


main:: IO()
main = do
        print (addOneToInt myIntList)
        print (absAll myIntList)
        print (squareAll myIntList)
