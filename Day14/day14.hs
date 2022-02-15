main :: IO()
main = putStr "What your name?"
       >> getLine
       >>= \a -> putStr "How old are you?"
       >> getLine
       >>= \b -> print(a,b)

main' :: IO()
main' = putStrLn "Hello" >> putStrLn "world!"

main3 :: IO()
main3 = do 
            putStr "What your name?"
            a <- getLine
            putStr "How old are you?"
            b <- getLine
            print(a,b)

main4:: IO()
main4 = do 
            let demoInt = read "100" :: Int
            let demoDouble = read "100" :: Double
            print demoDouble

readAInt :: IO Int
readAInt = readLn

readBBool :: IO Bool
readBBool = readLn

main5 :: IO ()
main5 = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn ( show (n+1))))

-- data Person = Person String Int

-- name :: Person -> String
-- name (Person x _) = x

-- age :: Person -> Int
-- age (Person _ y) = y

data Person = Person {name :: String, age :: Int } deriving Show

asep :: Person
asep = Person { name = "Asep", age = 24}

adit :: Person
adit = asep { name = "Adit" }

printName :: Person -> IO()
printName (Person {name = x}) = print x

