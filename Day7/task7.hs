data NonEmptyList a = Empty | NEL a [a] deriving Show

nelToList :: NonEmptyList a -> [a]
nelToList Empty = []
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel [] = Nothing
listToNel (x:xs) = Just (NEL x xs)

headNel :: NonEmptyList a -> a
headNel Empty = error "The argument must not be Empty"
headNel (NEL x xs) = x

tailNel :: NonEmptyList a -> [a]
tailNel Empty = error "The argument must not be Empty"
tailNel (NEL x xs) = xs

--  Result
outputTask2ver1 = nelToList (NEL 4 [1,2])
outputTask2ver2 = nelToList (NEL 7 [1,2])

outputTask3ver1 = listToNel [1,2]
outputTask3ver2 = listToNel [4,1,2]
outputTask3ver3 = listToNel [7,1,2]

outputTask4ver1 = headNel (NEL 2 [1,2])
outputTask4ver2 = headNel (NEL 1 [1,2])
outputTask4ver3 = headNel (NEL 4 [1,2])

outputTask5ver1 = tailNel (NEL 1 [1,2])
outputTask5ver2 = tailNel (NEL 2 [1,2])

a :: Int
a = 2

b :: Int
b = 4

result =  'a' : "asd"