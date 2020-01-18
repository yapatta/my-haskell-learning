import           System.Environment
import           Control.Monad

main :: IO ()
main = do
    tmp <- getLine
    let n          = read tmp
    let lineToRead = if n > 0 then n else 0 :: Int
    numbers <- replicateM lineToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)
