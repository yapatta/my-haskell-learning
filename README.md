# my-haskell-learning
Haskellの勉強記録. 
GHCのバージョンは, stack ghc 8.6.5です.
stack ghciとかで実行できます.
```
stack ghci
```
```
:l example.hs
```

## crypt.hs
Haskellを用いてrot, xor 線形合同法などを実装

## direct_sum.hs

直和型の例として, お店で売られている商品について実装.

### 直和型

直和型とは, Haskellにおける他の型とORで組み合わせることによって作成される型
具体例を見せると分かりやすいと思うので, 
```
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName
```
Name型は, メンバにFirstNameとLastNameを持っている場合と, FirstNameとLastName, さらにMiddleNameを持っている場合の両方を許容するという意味である. 

直積型のみを用いた実装でよくある階層的設計とおさらばできるらしい(オブジェクト指向の継承的なもの)

## semigroup.hs monoid.hsについて

半群とモノイドについて実装しました.
以下用語の説明です. 

### Compose

「.」という演算子を用いて, 関数を合成する. 
やっていることとしては,
```
f . g = (\x -> f (g x))
```
例として, 配列の最後の値を取る関数
```
myLast :: [a] -> a
myLast = head . reverse
```

### Magma(マグマ)

足し算あるいは掛け算ができる構造. 

例えば, Integer型では, 
```
instance Magma Integer where
 (<>) :: (+)
```

こうとも書ける.
```
instance Magma Integer where
 (<>) x y = x + y
```

ちなみに, この場合の型シグニチャは,
```
(<>) :: a -> a -> a
```

このとき, (<>)はaに閉じている(<>で演算しても, 型はaのまま).

### 半群(Semigroup)

マグマ + 演算が結合律に従う構造(左右どちらから演算しても変わらない).

例として, <>という演算子について, 
```
x <> (y <> z) == (x <> y) <> z
```
が成立する. 


### モノイド(Monoid)

単位元を要求するSemigroupといった印象. 
単位元とある値xを演算しても, xとなる.
```
e <> x == x <> e == x
```

```
class Monoid a where
 mempty :: a
 mappend :: a -> a -> a
 mconcat :: [a] -> a
```


## 二分探索木(binary_index_tree.hs)

大学の課題で二分探索木を作る必要があるので用意しました. 

### データ型
Tree a型: 自分自身の値a, left child, right childを持つ. or Empty(つまり, 子要素を何も持たない)

Emptyが等しいかのみを判別したEqについてのinstanceを作成.

### 関数群
コメントを参照

## maybe.hs
Maybe型についての実装

Maybe型とは: 欠損するかもしれない値をコンテキストで表すことができる型

他の言語のようなnull値に対するエラーで悩まされることから解放されるらしい...?

Maybe型を使えば, 例外が生じたとき, とりあえず値をNothingにして次に進めるのがいい. mapとかの作業途中に, エラーをキャッチしたせいで処理が終了するといったことがない. 