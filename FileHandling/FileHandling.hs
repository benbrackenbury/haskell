module FileHandling where
    main :: IO ()
    main =
        do
            text <- readFile "Test.hs"
            let output = process text
            writeFile "results.txt" output
    
    process :: String -> String
    process = unlines . filter notComment . lines

    notComment :: String -> Bool
    notComment comment =
        take 2 comment == "--"