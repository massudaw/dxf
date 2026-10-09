{-# LANGUAGE OverloadedStrings #-}
-- | A small JSON value, to dump what the library found so that the
-- oracle (ezdxf, in Python) can compare it with what it found.
module DXF.Json
  ( J (..), jsonOut, jObj, jPoint3, jPoint2
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Word (Word8)
import DXF.Vec

data J = JNull | JI !Int | JD !Double | JS !B.ByteString | JA [J] | JO [(B.ByteString, J)]
  deriving (Eq, Show)

jObj :: [(B.ByteString, J)] -> J
jObj = JO

jPoint3 :: V3 -> J
jPoint3 (V3 x y z) = JA [JD x, JD y, JD z]

jPoint2 :: V2 -> J
jPoint2 (V2 x y) = JA [JD x, JD y]

-- | Strings are the file's bytes, written as the code points Latin-1
-- gives them (the comparison does the same on its side).
jsonOut :: J -> BB.Builder
jsonOut j = case j of
  JNull -> "null"
  JI i -> BB.intDec i
  JD d | isNaN d || isInfinite d -> "null"
       | otherwise -> BB.doubleDec d
  JS s -> str s
  JA xs -> "[" <> commas (map jsonOut xs) <> "]"
  JO kvs -> "{" <> commas [str k <> ":" <> jsonOut v | (k, v) <- kvs] <> "}"
  where
    commas [] = mempty
    commas (x : xs) = x <> mconcat [BB.char7 ',' <> y | y <- xs]
    str s = BB.char7 '"' <> B.foldr (\w b -> esc w <> b) mempty s <> BB.char7 '"'
    esc :: Word8 -> BB.Builder
    esc w | w == 34 = "\\\""
          | w == 92 = "\\\\"
          | w < 32 || w >= 127 = "\\u00" <> hex (w `div` 16) <> hex (w `mod` 16)
          | otherwise = BB.word8 w
    hex d = BB.char7 ("0123456789abcdef" !! fromIntegral d)
