module Data.Strings where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- from String
stringToByteString :: String -> B.ByteString
stringToByteString = cs

stringToLazyByteString :: String -> BL.ByteString
stringToLazyByteString = cs

stringToText :: String -> T.Text
stringToText = cs

stringToLazyText :: String -> TL.Text
stringToLazyText = cs

-- fromByteString
byteStringToString :: B.ByteString -> String
byteStringToString = cs

byteStringToLazyByteString :: B.ByteString -> BL.ByteString
byteStringToLazyByteString = cs

byteStringToText :: B.ByteString -> T.Text
byteStringToText = cs

byteStringToLazyText :: B.ByteString -> TL.Text
byteStringToLazyText = cs

-- fromLazyByteString
lazyByteStringToString :: BL.ByteString -> String
lazyByteStringToString = cs

lazyByteStringToByteString :: BL.ByteString -> B.ByteString
lazyByteStringToByteString = cs

lazyByteStringToText :: BL.ByteString -> T.Text
lazyByteStringToText = cs

lazyByteStringToLazyText :: BL.ByteString -> TL.Text
lazyByteStringToLazyText = cs

-- fromText
textToString :: T.Text -> String
textToString = cs

textToByteString :: T.Text -> B.ByteString
textToByteString = cs

textToLazyByteString :: T.Text -> BL.ByteString
textToLazyByteString = cs

textToLazyText :: T.Text -> TL.Text
textToLazyText = cs

-- fromLazyText
lazyTextToString :: TL.Text -> String
lazyTextToString = cs

lazyTextToByteString :: TL.Text -> B.ByteString
lazyTextToByteString = cs

lazyTextToLazyByteString :: TL.Text -> BL.ByteString
lazyTextToLazyByteString = cs

lazyTextToText :: TL.Text -> T.Text
lazyTextToText = cs

-- Show
showByteString :: Show a => a -> B.ByteString
showByteString = stringToByteString . show

showLazyByteString :: Show a => a -> BL.ByteString
showLazyByteString = stringToLazyByteString . show

showText :: Show a => a -> T.Text
showText = stringToText . show

showLazyText :: Show a => a -> TL.Text
showLazyText = stringToLazyText . show
