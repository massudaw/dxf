{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
-- | Decimal to double, correctly rounded, in a few multiplications: the
-- Eisel-Lemire algorithm (Lemire 2021, "Number Parsing at a Gigabyte per
-- Second").  A decimal @w * 10^q@ with @w@ up to 19 digits becomes the
-- nearest double, or 'Nothing' in the rare cases the algorithm cannot
-- decide (subnormal and huge results, exact halfway-ish products); the
-- caller then takes the exact path.
module DXF.Number.Lemire
  ( eiselLemire
  ) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
import Data.Word (Word64)
import GHC.Exts (Word (..), timesWord2#)
import GHC.Float (castWord64ToDouble)

-- | The 128 bits of 5^q, normalised (high bit set), for q in -342..308 as
-- (high word, low word) pairs.  Negative powers are rounded up, the rest
-- truncated, as the algorithm requires.
table :: UArray Int Word
table = listArray (0, 2 * 651 - 1) (concatMap split entries)
  where
    split c = [fromInteger (c `shiftR` 64), fromInteger (c .&. (2 ^ (64 :: Int) - 1))]
    entries = [neg q | q <- [-342 .. -1]] ++ [pos q | q <- [0 .. 308]]
    neg :: Int -> Integer
    neg q =
      let p = 5 ^ negate q :: Integer
          z = length (takeWhile (< p) (iterate (* 2) 1)) :: Int
      in if q >= -27 then 2 ^ (z + 127) `div` p + 1
         else halve (2 ^ (2 * z + 128) `div` p + 1)
    halve c = if c >= 2 ^ (128 :: Int) then halve (c `div` 2) else c
    pos :: Int -> Integer
    pos q = down (up (5 ^ q))
    up x = if x < 2 ^ (127 :: Int) then up (x * 2) else x
    down x = if x >= 2 ^ (128 :: Int) then down (x `div` 2) else x :: Integer
{-# NOINLINE table #-}

-- | The 128-bit product of two words: (high, low).
mul128 :: Word -> Word -> (# Word, Word #)
mul128 (W# a) (W# b) = case timesWord2# a b of (# h, l #) -> (# W# h, W# l #)
{-# INLINE mul128 #-}

-- | @w * 10^q@ as a double, for @w > 0@.
eiselLemire :: Word64 -> Int -> Maybe Double
eiselLemire w0 q
  | q < -342 || q > 308 || w0 == 0 = Nothing
  | otherwise =
      let w = fromIntegral w0 :: Word
          lz = countLeadingZeros w
          wn = w `unsafeShiftL` lz
          idx = 2 * (q + 342)
          thi = unsafeAt table idx
          tlo = unsafeAt table (idx + 1)
      in case mul128 wn thi of
           (# h1, l1 #)
             | h1 .&. 0x1ff == 0x1ff -> case mul128 wn tlo of
                 (# h2, _ #) -> let l' = l1 + h2 in finish lz (if h2 > l' then h1 + 1 else h1) l'
             | otherwise -> finish lz h1 l1
  where
    finish :: Int -> Word -> Word -> Maybe Double
    finish !lz !hi !lo
      | lo == maxBound && (q < -27 || q > 55) = Nothing
      | p2 <= 0 || p2 >= 0x7ff = Nothing
      | p3 >= 0x7ff = Nothing
      | otherwise = Just (castWord64ToDouble (fromIntegral (fromIntegral p3 `unsafeShiftL` 52 .|. (m3 .&. 0xfffffffffffff))))
      where
        upper = fromIntegral (hi `unsafeShiftR` 63) :: Int
        sh = upper + 9
        m0 = hi `unsafeShiftR` sh
        p2 = (((152170 + 65536) * q) `shiftR` 16) + 63 + upper - lz + 1023 :: Int
        m1 | lo <= 1 && q >= -4 && q <= 23 && m0 .&. 3 == 1 && m0 `unsafeShiftL` sh == hi = m0 .&. complement 1
           | otherwise = m0
        m2 = (m1 + (m1 .&. 1)) `unsafeShiftR` 1
        (m3, p3) | m2 >= 0x20000000000000 = (0x10000000000000, p2 + 1)
                 | otherwise = (m2, p2)
