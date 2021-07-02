module Main where

    getForename :: IO String
    getForename =
        do
            putStrLn "Please enter your forename"
            fname <- getLine
            return fname

    getAndCalcSurname :: IO String
    getAndCalcSurname =
        do
            putStrLn "Enter your surname"
            sname <- getLine
            let
                num = length sname
                rev = reverse sname
            putStrLn ("Your name has " ++ show num
                        ++ " chars")
            putStrLn ("Your name in reverse is: "
                        ++ rev)
            return sname 


    main :: IO ()
    main =
        do
            fname <- getForename
            sname <- getAndCalcSurname
            putStrLn ("hello " ++ fname ++ " " ++ sname)