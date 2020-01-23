-- 整数nを与えたとき, 1からnまでの自然数ができる順列をすべて発生させるプログラム
select :: [Int] -> [(Int, [Int])]
select [x     ] = [(x, [])]
select (x : xs) = (x, xs) : map (\(y, ys) -> (y, x : ys)) (select xs)

permutation :: [Int] -> [[Int]]
permutation [] = [[]]
permutation xs =
    concat . map (\(y, ys) -> map (y :) $ permutation ys) $ select xs

makePermutationFromInt :: Int -> [[Int]]
makePermutationFromInt n = permutation [1 .. n]

-- ツリー型(要素がa)
data Tree a = Empty | Node Int (Tree a) (Tree a) deriving (Show)
instance Eq (Tree a) where
    (==) Empty Empty = True
    (==) (Node a leftA rightA) (Node b leftB rightB) =
        if (a == b) && (leftA == leftB) && (rightA == rightB)
            then True
            else False
    (==) _ _ = False

-- | 要素がTreeに含まれるか探索
treeElem :: Int -> Tree a -> Bool
treeElem _ Empty = False
treeElem x (Node a left right) | x == a    = True
                               | x < a     = treeElem x left
                               | otherwise = treeElem x right

-- | Treeの全体の高さを求める
getHeight :: Tree a -> Int
getHeight Empty               = 0
getHeight (Node _ left right) = max (getHeight left) (getHeight right) + 1

-- | Treeにxを挿入する
insert :: Tree a -> Int -> Tree a
insert Empty x = Node x Empty Empty
insert (Node a left right) x | x == a    = Node a left right
                             | x < a     = Node a (insert left x) right
                             | otherwise = Node a left (insert right x)

-- | Treeからxを削除する
delete :: Tree a -> Int -> Tree a
delete Empty x = Empty
delete (Node a left right) x
    | x == a = if (left == Empty && right == Empty)
        then Empty
        else if left == Empty
            then right
            else if right == Empty then left else delete2 (Node a left right)
    | x < a = Node a (delete left x) right
    | otherwise = Node a left (delete right x)

-- | 子が２つのときの削除
-- 左の子の最大値を親とした木を返す
delete2 :: Tree a -> Tree a
delete2 (Node a left right) =
    Node (getMaxChild left) (deleteMaxChild left) right

-- | 子の最大値をとる
getMaxChild :: Tree a -> Int
getMaxChild (Node a _    Empty) = a
getMaxChild (Node a left right) = getMaxChild right

-- | 最大の子を消した木を返す
deleteMaxChild :: Tree a -> Tree a
deleteMaxChild (Node a Empty Empty) = Empty
deleteMaxChild (Node a left  Empty) = left
deleteMaxChild (Node a left  right) = Node a left (deleteMaxChild right)

-- ここは間違って作ってしまったコーナー --
{--
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
--}
-- 間違って作ってしまったコーナー(終) --

-- Permutationから二分探索木を作成
makeBSTfromPermutation :: Int -> [Tree a]
makeBSTfromPermutation n = map (foldl insert Empty) (permutation [1 .. n])

-- 生成された二分探索木の個数
numBST :: [Tree a] -> Int
numBST a = length a

-- 生成された二分探索木の高さの合計
sumHeightBST :: [Tree a] -> Int
sumHeightBST a = foldl (+) 0 (map getHeight a)

-- 生成された二分探索木の高さの平均
averageHeightBST :: Fractional b => [Tree a] -> b
averageHeightBST a =
    (fromIntegral $ sumHeightBST a) / (fromIntegral $ numBST a)

-- 生成された二分探索木の高さの分散
varianceHeightBST :: Fractional b => [Tree a] -> b
varianceHeightBST a =
    (foldl (+) 0 (map (\x -> (fromIntegral (getHeight x) - average) ^ 2) a))
        / (fromIntegral $ numBST a)
    where average = averageHeightBST a

-- 生成された二分探索木の中で相異なる二分探索木を求める
uniqueBST :: (Eq a) => [a] -> [a]
uniqueBST []       = []
uniqueBST (x : xs) = (if x `elem` xs then [] else [x]) ++ (uniqueBST xs)


-- AVL木生成に関して --

-- 木の高さが1以下か確認
diffLessThan2 :: Tree a -> Bool
diffLessThan2 (Node a left right) =
    if (>) (abs ((-) (getHeight left) (getHeight right))) 1 then False else True

-- AVL木か
isAVL :: Tree a -> Bool
isAVL Empty               = True
isAVL (Node a left right) = if (diffLessThan2 (Node a left right)) == True
    then (isAVL left) && (isAVL right)
    else False

-- 生成された二分探索木からAVL木を抽出
avlTrees :: [Tree a] -> [Tree a]
avlTrees a = filter isAVL (uniqueBST a)


-- 実行例 --

-- 二分探索木作成の例
-- tree = foldl insert Empty [5,3,4,7,1,9]
{--
 -        5
 -       3  7
 -      1 4  9  
 -  みたいな木ができる
--}
-- 二分探索木の削除
-- 削除前
-- Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 7 Empty (Node 9 Empty Empty))
--
-- 削除
-- delete tree 3
-- Node 5 (Node 1 Empty (Node 4 Empty Empty)) (Node 7 Empty (Node 9 Empty Empty))
{--
 -        5
 -       1  7
 -        4  9  
 -  みたいな木ができる
--}


-- 課題1 --
-- 1..nまでの自然数から出来る順列をすべて発生
-- EX1: permutation [1..3]
-- EX2: makePermutationFromInt 3
-- 結果: [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]


-- 課題2-1 --
-- 二分探索木の個数(1..5)
-- EX: numBST $ makeBSTfromPermutation 5
-- 結果: 120

-- 二分探索木の高さの平均(1..5)
-- EX: averageHeightBST $ makeBSTfromPermutation 5
-- 結果: 3.8

-- 二分探索木の高さの分散(1..5)
-- EX: varianceHeightBST $ makeBSTfromPermutation 5
-- 結果: 0.4266666666666666


-- 課題2-2 --
-- 相異なる二分探索木の個数(1..3)
-- EX: numBST $ uniqueBST $ makeBSTfromPermutation 3
-- 結果: 5

-- 相異なる二分探索木の高さの平均(1..3)
-- EX: averageHeightBST $ uniqueBST $ makeBSTfromPermutation 3
-- 結果: 2.8

-- 相異なる二分探索木高さの分散
-- EX: varianceHeightBST $ uniqueBST $ makeBSTfromPermutation 3
-- 結果: 0.15999999999999998


-- 課題3 --
-- AVL木の個数(1..5)
-- EX: numBST $ avlTrees $ makeBSTfromPermutation 5
-- 結果: 6

-- AVL木の高さの平均
-- EX: averageHeightBST $ avlTrees $ makeBSTfromPermutation 5
-- 結果: 3.0

-- AVL木の高さの分散
-- EX: varianceHeightBST $ avlTrees $ makeBSTfromPermutation 5
-- 結果: 0.0

-- 木構造を可視化する機能(Optional) --
-- EX: avlTrees $ makeBSTfromPermutation 3
-- 結果: [Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)]
-- これは, 木の親が2という値を持っており, 左の子が1, 右の子が3を持っていることを意味する.
-- Emptyは何も存在しないことを表す. 
-- つまり, Node 3 Empty Empty とは, 左右に子が存在しないことを表す. 
