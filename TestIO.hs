module Main where

    main :: IO ()
    main = lineLen

    lineLen :: IO ()
    lineLen =
        do
            putStrLn "Enter some text"
            line <- getLine
            putStrLn ("There were "
                        ++ show (length line)
                        ++ " characters")