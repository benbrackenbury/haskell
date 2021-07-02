module GuessingGame where
    guessingGame :: Int -> IO ()
    guessingGame secret =
        do
            putStrLn "Input a number:"
            line <- getLine
            let number = read line :: Int
            case compare number secret of
                EQ -> putStrLn "Well doe"
                LT -> do
                    putStrLn "Too low"
                    guessingGame secret
                GT -> do
                    putStrLn "Too high"
                    guessingGame secret
