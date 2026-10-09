{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Binary DXF ("AutoCAD Binary DXF") to tags and back.
--
-- Binary values become the same text form a text file holds: integers in
-- decimal, doubles in their shortest round-tripping form, chunks of binary
-- data in hex.  So a 'Tag' means the same in both formats.
module DXF.Tag.Binary
  ( binarySignature, isBinary, scanBinary
  , binaryTag, binCode, binD, binI, binS
  ) where

import Control.Exception (throw)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as U
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word64)
import GHC.Float (castWord64ToDouble)
import DXF.Number
import DXF.Tag

binarySignature :: B.ByteString
binarySignature = BC.pack "AutoCAD Binary DXF\r\n\x1a\0"

isBinary :: B.ByteString -> Bool
isBinary = B.isPrefixOf binarySignature

strict :: BB.Builder -> B.ByteString
strict = BL.toStrict . BB.toLazyByteString

-- | R12 or older, from the $ACADVER found in the first kilobyte.
versionR12 :: B.ByteString -> Bool
versionR12 bs = case B.breakSubstring (BC.pack "$ACADVER") (B.take 1024 (B.drop 22 bs)) of
  (_, r) | B.null r -> True
         | otherwise ->
             let a = B.drop 10 r                     -- name, NUL, 1-byte code
                 v = if B.take 1 a == BC.pack "A" then B.take 6 a else B.take 6 (B.drop 1 a)
             in v <= BC.pack "AC1009"

-- | Tags of a binary file.
scanBinary :: B.ByteString -> (Fmt, [Tag])
scanBinary bs = (Fmt True False False r12, go 22)
  where
    n = B.length bs
    r12 = versionR12 bs
    u i = fromIntegral (U.unsafeIndex bs i) :: Int
    need i k = if i + k > n then throw (DxfError i "binary DXF: unexpected end of data") else ()
    go !i
      | i >= n = []
      | otherwise =
          let (c, j) | r12 = if u i == 255 then (u (i + 1) .|. (u (i + 2) `shiftL` 8), i + 3) else (u i, i + 1)
                     | otherwise = need i 2 `seq` (u i .|. (u (i + 1) `shiftL` 8), i + 2)
          in value c j
    value c j = case kindOf c of
      KBin -> need j 1 `seq`
        let len = u j in need (j + 1) len `seq`
           Tag c (hex (U.unsafeTake len (U.unsafeDrop (j + 1) bs))) : go (j + 1 + len)
      KInt16 -> need j 2 `seq` Tag c (dec (fromIntegral (fromIntegral (le j 2) :: Int16))) : go (j + 2)
      KInt32 -> need j 4 `seq` Tag c (dec (fromIntegral (fromIntegral (le j 4) :: Int32))) : go (j + 4)
      KInt64 -> need j 8 `seq` Tag c (dec (fromIntegral (fromIntegral (le j 8) :: Int64))) : go (j + 8)
      KBool -> need j 1 `seq` Tag c (dec (u j)) : go (j + 1)
      KDouble -> need j 8 `seq` Tag c (strict (BB.doubleDec (castWord64ToDouble (fromIntegral (le j 8))))) : go (j + 8)
      KStr -> case B.elemIndex 0 (U.unsafeDrop j bs) of
        Nothing -> throw (DxfError j "binary DXF: unterminated string")
        Just k -> Tag c (B.copy (U.unsafeTake k (U.unsafeDrop j bs))) : go (j + k + 1)
    le :: Int -> Int -> Word64
    le j k = foldr (\d a -> a `shiftL` 8 .|. fromIntegral (u (j + d))) 0 [0 .. k - 1]
    dec :: Int -> B.ByteString
    dec = strict . BB.intDec

hex :: B.ByteString -> B.ByteString
hex = strict . B.foldr (\w b -> hexDigit (w `shiftR` 4) <> hexDigit (w .&. 15) <> b) mempty
  where hexDigit d = BB.word8 (if d < 10 then 48 + d else 55 + d)

unhex :: B.ByteString -> B.ByteString
unhex s = fst (B.unfoldrN (B.length s `div` 2) step 0)
  where
    step i = Just (nib (B.index s i) `shiftL` 4 .|. nib (B.index s (i + 1)), i + 2)
    nib c | c >= 48 && c <= 57 = c - 48
          | c >= 65 && c <= 70 = c - 55
          | c >= 97 && c <= 102 = c - 87
          | otherwise = 0

-- | One tag in binary form.  Binary data longer than 127 bytes is split
-- into several tags, as the format needs.
binaryTag :: Bool -> Tag -> BB.Builder
binaryTag r12 (Tag c v) = case kindOf c of
  KBin -> chunks (unhex v)
  KDouble -> binD r12 c (maybe 0 id (readDouble v))
  KStr -> binS r12 c v
  _ -> binI r12 c (maybe 0 id (readInt v))
  where
    chunks d | B.null d = mempty
             | otherwise = let (a, b) = B.splitAt 127 d
                           in binCode r12 c <> BB.word8 (fromIntegral (B.length a)) <> BB.byteString a <> chunks b

-- | A group code.
binCode :: Bool -> Int -> BB.Builder
binCode r12 k
  | r12 = if k >= 255 then BB.word8 255 <> BB.word16LE (fromIntegral k) else BB.word8 (fromIntegral k)
  | otherwise = BB.word16LE (fromIntegral k)

binD :: Bool -> Int -> Double -> BB.Builder
binD r12 c d = binCode r12 c <> BB.doubleLE d

-- | An integer, of the width the group code has.
binI :: Bool -> Int -> Int -> BB.Builder
binI r12 c v = binCode r12 c <> case kindOf c of
  KInt16 -> BB.int16LE (fromIntegral v)
  KInt32 -> BB.int32LE (fromIntegral v)
  KInt64 -> BB.int64LE (fromIntegral v)
  _ -> BB.word8 (fromIntegral v)

binS :: Bool -> Int -> B.ByteString -> BB.Builder
binS r12 c s = binCode r12 c <> BB.byteString s <> BB.word8 0
