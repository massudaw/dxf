{-# LANGUAGE OverloadedStrings #-}
-- | Text in a DXF file is bytes in the file's encoding: UTF-8 from R2007,
-- a code page (header @$DWGCODEPAGE@) before.  The library keeps the bytes
-- and decodes only when asked; @\\U+XXXX@ escapes are resolved on the way.
module DXF.Encoding
  ( Encoding (..), encodingOf, decodeText, encodeText
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, isHexDigit, ord, toUpper, digitToInt, intToDigit)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

data Encoding = Utf8 | Latin1 | Cp1252
  deriving (Eq, Show)

-- | The encoding of a file: the DXF version (@AC1021@ and later are UTF-8)
-- and the @$DWGCODEPAGE@ value.  Code pages other than 1252 and the
-- Latin-1 family are read as Latin-1: ASCII is right, the rest is not.
encodingOf :: B.ByteString -> Maybe B.ByteString -> Encoding
encodingOf ver page
  | ver >= "AC1021" = Utf8
  | otherwise = case fmap (BC.map toUpper) page of
      Just p | p == "ANSI_65001" || p == "UTF-8" || p == "UTF8" -> Utf8
             | p == "ANSI_1252" -> Cp1252
      Nothing -> Cp1252
      _ -> Latin1

cp1252High :: [Char]
cp1252High = "\x20AC\xFFFD\x201A\x0192\x201E\x2026\x2020\x2021\x02C6\x2030\x0160\x2039\x0152\xFFFD\x017D\xFFFD\
             \\xFFFD\x2018\x2019\x201C\x201D\x2022\x2013\x2014\x02DC\x2122\x0161\x203A\x0153\xFFFD\x017E\x0178"

decodeText :: Encoding -> B.ByteString -> T.Text
decodeText enc bs = unescape $ case enc of
  Utf8 -> TE.decodeUtf8With TE.lenientDecode bs
  Latin1 -> TE.decodeLatin1 bs
  Cp1252 -> T.pack (map cp (B.unpack bs))
  where
    cp w | w >= 0x80 && w < 0xA0 = cp1252High !! fromIntegral (w - 0x80)
         | otherwise = chr (fromIntegral w)

-- | Resolve @\\U+XXXX@.
unescape :: T.Text -> T.Text
unescape t
  | not ("\\U+" `T.isInfixOf` t) = t
  | otherwise = T.concat (go t)
  where
    go s = case T.breakOn "\\U+" s of
      (a, r) | T.null r -> [a]
             | otherwise ->
                 let hx = T.take 4 (T.drop 3 r)
                 in if T.length hx == 4 && T.all isHexDigit hx
                      then a : T.singleton (chr (T.foldl' (\n c -> n * 16 + digitToInt c) 0 hx)) : go (T.drop 7 r)
                      else a : "\\U+" : go (T.drop 3 r)

-- | Text to the file's bytes; what the encoding cannot hold becomes @\\U+XXXX@.
encodeText :: Encoding -> T.Text -> B.ByteString
encodeText Utf8 = TE.encodeUtf8
encodeText enc = BC.concat . map one . T.unpack
  where
    one c | ord c < 0x80 = BC.singleton c
          | enc == Latin1 && ord c < 0x100 = BC.singleton c
          | enc == Cp1252, ord c < 0x100, not (ord c >= 0x80 && ord c < 0xA0) = BC.singleton c
          | enc == Cp1252, Just i <- lookup c (zip cp1252High [0 ..]) = B.singleton (0x80 + i)
          | otherwise = BC.pack ("\\U+" ++ hex4 (ord c))
    hex4 n = [intToDigit ((n `div` 4096) `mod` 16), intToDigit ((n `div` 256) `mod` 16), intToDigit ((n `div` 16) `mod` 16), intToDigit (n `mod` 16)]
