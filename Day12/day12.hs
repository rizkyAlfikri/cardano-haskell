import System.Directory
import Data.Char


data ExpT = Lit Int
    | Add ExpT ExpT
    | Mul ExpT ExpT
    deriving (Show, Eq)

-- eval' :: ExpT -> Int
-- eval' (Mul (Add (Lit a) (Lit b)) (Lit c)) = (a+b) * c

-- eval :: ExpT -> Int
-- eval (Lit i) = i
-- eval (Add e1 e2) = eval e1 + eval e2
-- eval (Mul e1 e2) = eval e1 * eval e2

-- -- constanta
-- e1 :: ExpT
-- e1 = Add (Lit 1)
--         (Add (Lit 2)
--              (Lit 3))

-- view:: ExpT -> String
-- view (Lit n) = show n
-- view (Add x y) = "(" ++ view x ++ " + " ++ view y ++ ")"
-- view (Mul x y) = "(" ++ view x ++ " x " ++ view y ++ ")"

reify :: ExpT -> ExpT
reify = id

data List t = E | C t (List t)

checkIsEmpty :: List t -> Maybe t
checkIsEmpty E = Nothing
checkIsEmpty (C x _) = Just x

-- Password
getPassPharse :: IO (Maybe  String)
getPassPharse = do s <- getLine
                   if isValid s then return $ Just s
                                 else return Nothing

isValid :: String -> Bool
isValid s = isContainLowerCase && isTotalDigitValid && isContainUpperCase && isContainDigit
    where
            isContainLowerCase = all isLower s
            isTotalDigitValid = length s >= 8
            isContainUpperCase = all isUpper s
            isContainDigit = all isDigit s
            isContainSpecilCharacter = map "/[!@#$%^&*()_+-=\[\]{};':\\|,.<>\/?]+/"


askUserIdPass :: IO()
askUserIdPass = do putStrLn  "Insert your username"
                   user <- getLine
                   putStrLn  "Insert your password"
                   maybe_pass <- getPassPharse
                   case maybe_pass of
                        Just value -> storeFile user value
                        Nothing -> putStrLn "Password invalid"

storeFile :: String -> String -> IO()
storeFile user pass =  do
            putStrLn "Storing in file ..."
            let filename = "listuserpass.txt"
            isFileExit <-  doesFileExist filename
            if isFileExit then putStrLn "" else writeFile filename ""
            contents <- readFile filename
            putStrLn contents
            appendFile filename $ "User : " ++ user ++ "\n" ++ "Pass : " ++ pass ++ "\n"

insertNewList :: [String] -> IO()
insertNewList oldValue = do
                            putStrLn "Masukan Data"
                            newValue <- getLine
                            let allList = oldValue ++ [newValue]
                            print allList
                            insertNewList allList

main :: IO()
main = insertNewList []