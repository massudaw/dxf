{-# LANGUAGE BangPatterns, LinearTypes, OverloadedStrings #-}
-- | The values of a binary DXF as the text a text DXF holds them in, each made straight from its number: one
-- small pinned array per value (linbuf's 'runPinned', frozen without a copy), or none at all -- the integers
-- 0 .. 999 are slices of one constant, and zero, the infinities and NaN are constants.
--
-- The text is what it always was (GHC's 'show', which @Data.ByteString.Builder.doubleDec@ writes): integers in
-- decimal; a double's shortest digits (linbuf's 'shortestDigits'), plain from 0.1 up to 10^7 (@0.25@,
-- @420.0@), else with an exponent (@1.0e-2@, @1.0e20@).  Before, each value went through a 'BB.Builder' run
-- to a lazy chunk of 4 KB and its copy -- 570 bytes allocated for each byte read (PERF.md).
module DXF.Tag.BinText (intText, showDouble) where

import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64, Word8)
import qualified LinBuf.Fixed as F
import LinBuf.Index (index8, sliceAt)
import LinBuf.Real (shortestDigits, wordDecPadded)
import LinBuf.Write (Buf, Ur (..), bytes, decimal, freezeBytes, putBounded, runPinned, u8)

-- | An integer in decimal.
intText :: Int -> B.ByteString
intText i
  | i >= 0 && i < 10 = sliceAt smallInts i 1
  | i >= 0 && i < 100 = sliceAt smallInts (10 + 2 * (i - 10)) 2
  | i >= 0 && i < 1000 = sliceAt smallInts (190 + 3 * (i - 100)) 3
  | otherwise = runPinned 20 (\b -> freezeBytes (decimal i b))
{-# INLINE intText #-}

-- | "0123456789101112...999": every integer below 1000, its text a slice.
smallInts :: B.ByteString
smallInts = BL.toStrict (BB.toLazyByteString (foldMap BB.intDec [0 .. 999 :: Int]))
{-# NOINLINE smallInts #-}

-- | A double as 'show' writes it.
--
-- 'shortestDigits' (Schubfach) and 'show' (Dragon4, as bytestring's 'BB.doubleDec' keeps it) take the same
-- digits but in two cases: a candidate exactly on the edge of the double's rounding interval (Schubfach takes
-- it when the mantissa is even, 'show' never: @1e23@ is @9.999999999999999e22@), and two candidates as close
-- (Schubfach takes the even one, 'show' the upper: @12.566970825195312|3@).  Either needs @m * 10^e@ to be a
-- dyadic of 53 bits: with @e < 0@, @m >= 2^52@ and @5^-e@ dividing @m@ or @2m +- 1@; with @e >= 0@, a double of
-- @2^52@ or more, or @m >= 2^52@.  Those few go the old way; every other double is laid out from its digits.
showDouble :: Double -> B.ByteString
showDouble x
  | isNaN x = "NaN"
  | isInfinite x = if x > 0 then "Infinity" else "-Infinity"
  | x == 0 = if isNegativeZero x then "-0.0" else "0.0"
  | otherwise = case shortestDigits a of
      (!m, !n, !e)
        | doubtful m e -> BL.toStrict (BE.toLazyByteStringWith (BE.untrimmedStrategy 32 32) BL.empty (BB.doubleDec x))
        | otherwise -> runPinned 32 (\b -> freezeBytes (layout m n (n + e) (sign (x < 0) b)))
  where
    !a = abs x
    doubtful m e
      | m < 1000000000000000 = e >= 0 && a >= 4503599627370496
      | e >= 0 = True
      | e < -24 = False
      | otherwise = let !q = 5 ^ negate e :: Word64
                    in m `rem` q == 0 || (2 * m + 1) `rem` q == 0 || (2 * m - 1) `rem` q == 0

sign :: Bool -> Buf t %1 -> Buf t
sign True b = u8 45 b
sign False b = b

-- | The digits @m@ (@n@ of them) of @0.m * 10^k@: plain when @0 <= k <= 7@, else @d.ddde(k-1)@.
layout :: Word64 -> Int -> Int -> Buf t %1 -> Buf t
layout m n k b
  | k == 0 = dec m (bytes "0." b)
  | k > 0 && k <= 7 && k >= n = bytes ".0" (dec (m * p10 (k - n)) b)
  | k > 0 && k <= 7 = let !q = p10 (n - k) in pad (n - k) (m `rem` q) (u8 46 (dec (m `quot` q) b))
  | n == 1 = decimal (k - 1) (bytes ".0e" (dec m b))
  | otherwise = let !q = p10 (n - 1) in decimal (k - 1) (u8 101 (pad (n - 1) (m `rem` q) (u8 46 (dec (m `quot` q) b))))

dec :: Word64 -> Buf t %1 -> Buf t
dec w = putBounded (F.wordDec (fromIntegral w))
{-# INLINE dec #-}

pad :: Int -> Word64 -> Buf t %1 -> Buf t
pad k w = putBounded (wordDecPadded k (fromIntegral w))
{-# INLINE pad #-}

p10 :: Int -> Word64
p10 k = 10 ^ k
