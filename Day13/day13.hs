main :: IO()
main = addList []

addList :: [String ] -> IO()
addList oldValue = do
                     putStrLn "\na > AddNewList \nd> Done"
                     choseMenu <- getLine
                     case choseMenu of
                         "a" -> do
                                    putStr "Insert new value : "
                                    newValue <- getLine 
                                    let newValueList = oldValue ++ [newValue]
                                    print newValueList
                                    addList newValueList
                         "d" -> print oldValue
                         _   -> do 
                                    putStrLn "Please choose first letter from menu \na > AddNewList \nd> Done"
                                    print oldValue
                                    addList oldValue