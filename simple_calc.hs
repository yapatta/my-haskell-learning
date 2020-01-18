sampleInput :: [String]
sampleInput = ["21", "+", "123"]

calc :: [String] -> Int
calc (a : "+" : b : rest) = (read a) + (read b)
calc (a : "*" : b : rest) = (read a) * (read b)

main :: IO ()
main = do
    tmp <- getContents
    let values = lines tmp
    print (calc values)
