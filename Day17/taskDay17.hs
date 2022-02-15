prompt :: String  -> IO (IO())
prompt param = do
                putStrLn ("What is your " ++ param ++ " ?")
                x <- getLine
                return (putStrLn ("Your " ++ param ++ " is : " ++ x))

runWizard :: IO (IO a) -> IO a
runWizard request = do
                respond <- request
                respond

main :: IO ()
main = runWizard (prompt "name" <> prompt "age" <> prompt "color")
                