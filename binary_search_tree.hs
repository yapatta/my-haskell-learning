import           Control.Monad
import           Control.Applicative

-- ツリー型(要素がa)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
instance Eq (Tree a) where
    (==) Empty Empty = True
    (==) _     _     = False

-- | 要素がTreeに含まれるか探索
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ Empty = False
treeElem x (Node a left right) | x == a    = True
                               | x < a     = treeElem x left
                               | otherwise = treeElem x right

-- | Treeの全体の高さを求める
getHeight :: Tree a -> Int
getHeight Empty               = 0
getHeight (Node _ left right) = max (getHeight left) (getHeight right) + 1

-- | Treeにxを挿入する
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = Node x Empty Empty
insert (Node a left right) x | x == a    = Node a left right
                             | x < a     = Node a (insert left x) right
                             | otherwise = Node a left (insert right x)

-- | 木の高さの合計を求める
heightSum :: Int -> Tree a -> Int
heightSum depth (Node a left right)
    | left == Empty && right == Empty
    = depth
    | left == Empty
    = (heightSum (depth + 1) right) + depth
    | right == Empty
    = (heightSum (depth + 1) left) + depth
    | otherwise
    = (heightSum (depth + 1) left) + (heightSum (depth + 1) right) + depth

-- | 木の要素の個数を求める
countNum :: Tree a -> Int
countNum Empty = 0
countNum (Node a left right)
    | left == Empty && right == Empty = 1
    | left == Empty                   = (countNum right) + 1
    | right == Empty                  = (countNum left) + 1
    | otherwise                       = (countNum left) + (countNum right) + 1

-- | 木の高さの平均を求める
heightAverage :: (Fractional p) => Tree a -> p
heightAverage Empty = 0
heightAverage tree =
    (fromIntegral (heightSum 0 tree)) / (fromIntegral (countNum tree))

-- 例
-- tree = foldl insert Empty [5,3,7,1,9]
{-
 -        5
 -       3 7
 -      1   9  
 -  みたいな木ができる
 -}
