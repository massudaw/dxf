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
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import LinBuf.Index (index8, index16le, index32le, index64le, sliceAt)
import Data.Int (Int16, Int32, Int64)
import GHC.Float (castWord64ToDouble)
import DXF.Number
import DXF.Tag
import DXF.Tag.BinText (hexText, intText, showDouble)

binarySignature :: B.ByteString
binarySignature = BC.pack "AutoCAD Binary DXF\r\n\x1a\0"

isBinary :: B.ByteString -> Bool
isBinary = B.isPrefixOf binarySignature

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
    u i = fromIntegral (index8 bs i) :: Int
    need i k = if i + k > n then throw (DxfError i "binary DXF: unexpected end of data") else ()
    go !i
      | i >= n = []
      | otherwise =
          if r12
            then if u i == 255 then value (u (i + 1) .|. (u (i + 2) `shiftL` 8)) (i + 3) else value (u i) (i + 1)
            else need i 2 `seq` value (u i .|. (u (i + 1) `shiftL` 8)) (i + 2)
    -- Each tag is made (its bytes read) before it is consed: the list is lazy, its tags are not.
    value !c !j = case kindOf c of
    -- A value's text is made from its number ('DXF.Tag.BinText'); a string is a slice of the input.
      KBin -> need j 1 `seq`
        let !len = u j in need (j + 1) len `seq`
           emit (Tag c (hexText (sliceAt bs (j + 1) len))) (j + 1 + len)
      KInt16 -> need j 2 `seq` emit (Tag c (intText (fromIntegral (fromIntegral (index16le bs j) :: Int16)))) (j + 2)
      KInt32 -> need j 4 `seq` emit (Tag c (intText (fromIntegral (fromIntegral (index32le bs j) :: Int32)))) (j + 4)
      KInt64 -> need j 8 `seq` emit (Tag c (intText (fromIntegral (fromIntegral (index64le bs j) :: Int64)))) (j + 8)
      KBool -> need j 1 `seq` emit (Tag c (intText (u j))) (j + 1)
      KDouble -> need j 8 `seq` emit (Tag c (showDouble (castWord64ToDouble (index64le bs j)))) (j + 8)
      KStr -> case B.elemIndex 0 (B.drop j bs) of
        Nothing -> throw (DxfError j "binary DXF: unterminated string")
        Just k -> emit (Tag c (sliceAt bs j k)) (j + k + 1)
    emit !t !k = t : go k

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
  KDouble -> let !d = maybe 0 id (readDouble v) in binD r12 c d
  KStr -> binS r12 c v
  _ -> let !i = maybe 0 id (readInt v) in binI r12 c i
  where
    chunks d | B.null d = mempty
             | otherwise = let (a, b) = B.splitAt 127 d
                           in binCode r12 c <> BB.word8 (fromIntegral (B.length a)) <> BB.byteString a <> chunks b

-- | A group code.
binCode :: Bool -> Int -> BB.Builder
binCode r12 !k
  | r12 = if k >= 255 then BB.word8 255 <> BB.word16LE w else BB.word8 (fromIntegral k)
  | otherwise = BB.word16LE w
  where !w = fromIntegral k

binD :: Bool -> Int -> Double -> BB.Builder
binD r12 c !d = binCode r12 c <> BB.doubleLE d

-- | An integer, of the width the group code has.
binI :: Bool -> Int -> Int -> BB.Builder
binI r12 !c !v = binCode r12 c <> case kindOf c of
  KInt16 -> let !x = fromIntegral v in BB.int16LE x
  KInt32 -> let !x = fromIntegral v in BB.int32LE x
  KInt64 -> let !x = fromIntegral v in BB.int64LE x
  _ -> let !x = fromIntegral v in BB.word8 x

binS :: Bool -> Int -> B.ByteString -> BB.Builder
binS r12 c s = binCode r12 c <> BB.byteString s <> BB.word8 0
