module IO where

printTwoThings :: IO ()
printTwoThings = do
    putStrLn  "Hello!"
    putStrLn  "How are you?"

greet :: IO ()
greet = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)

printAndIncrement :: Int -> Int
printAndIncrement x = x+1
  where action = putStrLn "got a number!"