quotes :: [String]
quotes = ["quote 1", "quote 2", "quote 3", "quote 4", "quote 5"]

lookUpQuote :: [String] -> [String]
lookUpQuote []         = []
lookUpQuote ("n" : xs) = []
lookUpQuote (x   : xs) = quote : (lookUpQuote xs)
    where quote = quotes !! (read x - 1)

main :: IO ()
main = do
    putStrLn "1~5の数字を入力して下さい"
    userInput <- getContents
    mapM_ putStrLn (lookUpQuote (lines userInput))
