{-# LANGUAGE BangPatterns, MagicHash, UnboxedSums, UnboxedTuples, OverloadedStrings #-}
-- | Numbers parsed straight from the bytes of a tag value: no 'String',
-- no 'read'.  'readDouble' is correctly rounded: it is linbuf's
-- 'LinBuf.Decimal' with DXF's blanks and @D@ exponents around it.
module DXF.Number
  ( readDouble, readDoubleU, readInt, readHex
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import LinBuf.Index (peek8)
import qualified LinBuf.Decimal as LD
import Data.Bits (shiftL, (.|.))
import GHC.Exts (Double (D#), Double#)

isSp :: Int -> Bool
isSp c = c == 32 || c == 9 || c == 13 || c == 10
{-# INLINE isSp #-}

-- The scanners read with 'peek8': the byte, or -1 past the end -- which no
-- guard below accepts (not a digit, sign, point, exponent letter or blank),
-- so one unsigned comparison is both the loop's end and the read's check.

-- | Decimal integer with optional sign and surrounding blanks.  A value
-- written as a float ("12.0", some exporters do that) is truncated.
readInt :: B.ByteString -> Maybe Int
readInt bs = start 0
  where
    start !i = case peek8 bs i of
      c | c < 0 -> Nothing
        | isSp c -> start (i + 1)
        | c == 45 -> digits (i + 1) True 0 (i + 1)
        | c == 43 -> digits (i + 1) False 0 (i + 1)
        | otherwise -> digits i False 0 i
    digits !i !neg !acc !s
      | let c = peek8 bs i, c >= 48, c <= 57 = digits (i + 1) neg (acc * 10 + c - 48) s
      | i == s = Nothing
      | otherwise = rest i (if neg then negate acc else acc)
    rest !i !v = case peek8 bs i of
      c | c < 0 -> Just v
        | isSp c -> rest (i + 1) v
        | otherwise -> truncate <$> readDouble bs

-- | Hexadecimal (handles): upper or lower case, no prefix.
readHex :: B.ByteString -> Maybe Int
readHex bs
  | B.null bs || B.length bs > 15 = Nothing
  | otherwise = B.foldl' step (Just 0) bs
  where
    step Nothing _ = Nothing
    step (Just a) c
      | c >= 48 && c <= 57 = Just (a `shiftL` 4 .|. fromIntegral (c - 48))
      | c >= 65 && c <= 70 = Just (a `shiftL` 4 .|. fromIntegral (c - 55))
      | c >= 97 && c <= 102 = Just (a `shiftL` 4 .|. fromIntegral (c - 87))
      | otherwise = Nothing

-- | A floating point number: sign, digits, optional fraction, optional
-- exponent, blanks around.  'Nothing' when the bytes are not a number.
readDouble :: B.ByteString -> Maybe Double
readDouble bs = case readDoubleU bs of
  (# (# #) | #) -> Nothing
  (# | d #) -> Just (D# d)
{-# INLINE readDouble #-}

-- | 'readDouble' without a box for the answer: the unboxed sum is returned
-- in registers, so reading a number allocates nothing.
--
-- The number itself is linbuf's ('LinBuf.Decimal.scanDouble': Clinger's
-- exact path, Eisel-Lemire, 'Integer' when neither decides -- correctly
-- rounded for any count of digits); what is DXF's is around it: blanks
-- before and after, and a FORTRAN @D@ exponent (rare: the bytes are copied
-- with an @e@ in its place).
readDoubleU :: B.ByteString -> (# (# #) | Double# #)
readDoubleU bs = skip 0
  where
    skip !i = case peek8 bs i of
      c | c < 0 -> (# (# #) | #)
        | isSp c -> skip (i + 1)
        | otherwise -> number i
    number !i = case LD.scanDouble (BU.unsafeDrop i bs) of
      Nothing -> (# (# #) | #)
      Just (D# d, k)
        | blanks (i + k) -> (# | d #)
        | let c = peek8 bs (i + k), c == 68 || c == 100 -> dExponent i (i + k)
        | otherwise -> (# (# #) | #)
    blanks !i = let c = peek8 bs i in c < 0 || (isSp c && blanks (i + 1))
    dExponent i j = case LD.scanDouble e of
      Just (D# d, k) | blanks' k -> (# | d #)
      _ -> (# (# #) | #)
      where
        e = B.concat [B.take (j - i) (B.drop i bs), "e", B.drop (j + 1) bs]
        blanks' !m = let c = peek8 e m in c < 0 || (isSp c && blanks' (m + 1))
{-# INLINE readDoubleU #-}