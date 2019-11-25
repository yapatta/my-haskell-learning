data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    rotation     = ((fromEnum c) + halfAlphabet) `mod` alphabetSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
-- 奇数サイズならsize/2 + 1を
-- 偶数サイズならsize/2を
rotNdecoder n c = toEnum rotation
  where
    halfN    = n `div` 2
    offset   = if even n then fromEnum c + halfN else fromEnum c + halfN + 1
    rotation = offset `mod` n
{-
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    rotation =
        (if even n then fromEnum c + halfN else fromEnum c + halfN + 1) `mod` n
-}
largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

rotCharDecoder :: Char -> Char
rotCharDecoder charToEncrypt = rotNdecoder sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1, L2, L3, L4, L1, L1, L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4l     = rotN alphaSize

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3l     = rotN alphaSize

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3lDecode vals
  where
    alphaSize   = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3lDecode = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text

type Bits = [Bool]
intToBits' :: Int -> [Bool]
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal   = n `div` 2

intToBitsReverse' :: Int -> [Bool]
intToBitsReverse' n = reverse (intToBits' n)

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits  = reverse (intToBits' n)
    missingBits   = maxBits - (length reversedBits)
    leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: [Bool] -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
  where
    size          = length bits
    indices       = [size - 1, size - 2 .. 0]
    trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


-- ワンタイムパッド
myPad :: String
myPad = "Shhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xorBits` (snd pair))
                              (zip padBits plaintextBits)
  where
    padBits       = map charToBits pad
    plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
    where bitList = applyOTP' pad plaintext
xor :: Bool -> Bool -> Bool
xor left right = if left == right then False else True

xorBits :: Bits -> Bits -> Bits
xorBits []       []       = []
xorBits (x : xs) (y : ys) = (x `xor` y) : (xorBits xs ys)

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
 encode :: a -> String -> String
 decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100
