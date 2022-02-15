
data Student = Student {name :: String, grade :: Int, score :: Int } deriving Show

readAInt :: IO Int
readAInt = readLn

calculateSpacePrefix :: Int -> String
calculateSpacePrefix i = replicate i ' '

calculateSpaceSufix :: Int -> Int -> String
calculateSpaceSufix i lengthChar = replicate (i - lengthChar) ' '

headerLine = "---------------------------------------------------------------------\n"
headerData = "No.  |    Kelas     |     Grade     |     Nilai     |     Lulus     |\n"

main :: IO()
main = addList []

addList :: [Student] -> IO()
addList oldValue = do
                     putStrLn "Welcome on Student Dashboard"
                     putStrLn "\na > Add Student and score \nb > Print List of Student \nq > Done\n"
                     choseMenu <- getLine
                     case choseMenu of
                         "a" -> do
                                    putStr "Insert student name : "
                                    newName <- getLine
                                    putStr "Insert student grade : "
                                    newGrade <-  readAInt
                                    putStr "Insert student score : "
                                    newScore <- readAInt
                                    let newStudent = Student { name = newName, grade = newGrade, score = newScore}
                                    let newStudentList = oldValue ++ [newStudent]
                                    print newStudentList
                                    addList newStudentList
                         "b" -> putStr (printStudent 1 "" oldValue)
                         "q" -> print oldValue
                         _   -> do
                                    putStrLn "Invalid input\na > Add Student and score \nb > Print List of Student \nq > Done\n"
                                    print oldValue
                                    addList oldValue

printStudent :: Int -> String  -> [Student] -> String 
printStudent _ previousData [] = headerLine ++ headerData ++ headerLine ++ previousData ++ headerLine 
printStudent index previousData (x:xs) = do
                         let studentName = name x
                         let studentGrade = grade x
                         let studentScore = score x
                         let studentResult = if score x > 60 then "Y" else "N"
                         let printResult = show index ++ "." ++ calculateSpaceSufix 4 (length $ show index) ++ "|" ++ calculateSpacePrefix 4 ++ studentName ++ calculateSpaceSufix 10 (length studentName) ++ "|" ++ calculateSpacePrefix 5 ++ show studentGrade ++ calculateSpaceSufix 10 (length $ show studentGrade)  ++ "|" ++ calculateSpacePrefix 5 ++ show studentScore ++ calculateSpaceSufix 10 (length $ show studentScore) ++ "|" ++ calculateSpacePrefix 5 ++ studentResult ++ calculateSpaceSufix 12 (length $ show studentResult) ++ "|\n"
                         printStudent (index + 1) (previousData ++ printResult) xs
