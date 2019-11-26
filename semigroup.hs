import           Data.List
import           Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testfunc = (foldr (&&) True) . (map testfunc)

instance Semigroup Integer where
    (<>) x y = x + y

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red    Blue   = Purple
    (<>) Blue   Red    = Purple
    (<>) Yellow Blue   = Green
    (<>) Blue   Yellow = Green
    (<>) Yellow Red    = Orange
    (<>) Red    Yellow = Orange
    (<>) a b | a == b    = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | otherwise = Brown
