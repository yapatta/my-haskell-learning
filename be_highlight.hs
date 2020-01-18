{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

dharma :: T.Text
dharma = "dog"

hgText :: T.Text
hgText =
    "I have the dog which is very delicious. However, I refrain from eating this dog."

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces      = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]

main = do
 TIO.putStrLn (highlight dharma hgText)
