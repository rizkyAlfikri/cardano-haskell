exampleList = [1,2,3,4,5,6,7,8,9]

keepOnlyOddValue :: [Integer] -> [Integer]
keepOnlyOddValue [] = []
keepOnlyOddValue (x:xs)
    | odd x = x: keepOnlyOddValue xs
    | otherwise = keepOnlyOddValue xs


filterValue :: [Char] -> [Integer] -> [Integer]
filterValue _ [] = []
filterValue key (x:xs)
    | isEven = filterEven
    | isOdd = filterOdd
    | otherwise = []
    where isEven = "even" == key
          isOdd = "odd" == key
          filterEven = if even x then x : filterValue key xs else filterValue key xs
          filterOdd = if odd x then x : filterValue key xs else filterValue key xs

accumulateRec :: (a -> b) -> [a] -> [b]
accumulateRec _ [] = []
accumulateRec f (x:xs) = f x : accumulateRec f xs
