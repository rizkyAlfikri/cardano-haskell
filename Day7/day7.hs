-- Maybe

f:: Int -> Maybe Int
f 0 = Nothing 
f x = Just x

main = do
    let value = 2
    putStrLn "The result of the variable is "
    let result1 = maybe False odd (Just value)
    print("Result of value 1 is : ", value)

test :: Int -> Maybe Int
test x = if x > 0 then Just x else Nothing

extractAdd :: Maybe Int -> Int
extractAdd Nothing  = 0
extractAdd (Just x) = x + 10

cekData :: Int -> IO()
cekData z = do
    let xx = test z
    let zz = extractAdd xx
    print ("Nilainya = " ++ show zz)