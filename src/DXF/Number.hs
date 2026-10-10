{-# LANGUAGE BangPatterns, MagicHash, UnboxedSums, UnboxedTuples #-}
-- | Numbers parsed straight from the bytes of a tag value: no 'String',
-- no 'read'.  'readDouble' is correctly rounded: the fast path is exact
-- (mantissa below 2^53, decimal exponent within +-22, one IEEE operation),
-- every other number goes through exact 'Integer' arithmetic.
module DXF.Number
  ( readDouble, readDoubleU, readInt, readHex
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as U
import Data.Bits (shiftL, (.|.))
import Data.Ratio ((%))
import Data.Word (Word64)
import GHC.Exts (Double (D#), Double#)
import DXF.Number.Lemire (eiselLemire)

isSp :: Int -> Bool
isSp c = c == 32 || c == 9 || c == 13 || c == 10
{-# INLINE isSp #-}

at :: B.ByteString -> Int -> Int
at bs i = fromIntegral (U.unsafeIndex bs i)
{-# INLINE at #-}

-- | Decimal integer with optional sign and surrounding blanks.  A value
-- written as a float ("12.0", some exporters do that) is truncated.
readInt :: B.ByteString -> Maybe Int
readInt bs = start 0
  where
    n = B.length bs
    start !i
      | i >= n = Nothing
      | isSp (at bs i) = start (i + 1)
      | at bs i == 45 = digits (i + 1) True 0 (i + 1)
      | at bs i == 43 = digits (i + 1) False 0 (i + 1)
      | otherwise = digits i False 0 i
    digits !i !neg !acc !s
      | i < n, let c = at bs i, c >= 48, c <= 57 = digits (i + 1) neg (acc * 10 + c - 48) s
      | i == s = Nothing
      | otherwise = rest i (if neg then negate acc else acc)
    rest !i !v
      | i >= n = Just v
      | isSp (at bs i) = rest (i + 1) v
      | otherwise = truncate <$> readDouble bs

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

pow10 :: Int -> Double
pow10 k = case k of
  0 -> 1e0; 1 -> 1e1; 2 -> 1e2; 3 -> 1e3; 4 -> 1e4; 5 -> 1e5; 6 -> 1e6
  7 -> 1e7; 8 -> 1e8; 9 -> 1e9; 10 -> 1e10; 11 -> 1e11; 12 -> 1e12
  13 -> 1e13; 14 -> 1e14; 15 -> 1e15; 16 -> 1e16; 17 -> 1e17; 18 -> 1e18
  19 -> 1e19; 20 -> 1e20; 21 -> 1e21; _ -> 1e22

-- | A floating point number: sign, digits, optional fraction, optional
-- exponent, blanks around.  'Nothing' when the bytes are not a number.
readDouble :: B.ByteString -> Maybe Double
readDouble bs = case readDoubleU bs of
  (# (# #) | #) -> Nothing
  (# | d #) -> Just (D# d)
{-# INLINE readDouble #-}

-- | 'readDouble' without a box for the answer: the unboxed sum is returned
-- in registers, so reading a number allocates nothing.
readDoubleU :: B.ByteString -> (# (# #) | Double# #)
readDoubleU bs = rdStart bs 0

-- The scanner is a set of top-level functions with every value passed in
-- an argument: no closure is built per number.

rdStart :: B.ByteString -> Int -> (# (# #) | Double# #)
rdStart bs !i
  | i >= B.length bs = (# (# #) | #)
  | isSp (at bs i) = rdStart bs (i + 1)
  | at bs i == 45 = rdInts bs (i + 1) 0 0 0 True i
  | at bs i == 43 = rdInts bs (i + 1) 0 0 0 False i
  | otherwise = rdInts bs i 0 0 0 False i

-- integer part; m = mantissa, nd = significant digits kept, any_ = digits seen
rdInts :: B.ByteString -> Int -> Int -> Int -> Int -> Bool -> Int -> (# (# #) | Double# #)
rdInts bs !j !m !nd !any_ !neg !s0
  | j < n, let c = at bs j, c >= 48, c <= 57 =
      if nd >= 18 then rdSlow bs neg s0
      else let nd' = if m == 0 && c == 48 then nd else nd + 1
           in rdInts bs (j + 1) (m * 10 + fromIntegral (c - 48)) nd' (any_ + 1) neg s0
  | j < n, at bs j == 46 = rdFrac bs (j + 1) m nd any_ 0 neg s0
  | otherwise = rdExpo bs j m any_ 0 neg
  where n = B.length bs

rdFrac :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> Bool -> Int -> (# (# #) | Double# #)
rdFrac bs !j !m !nd !any_ !fd !neg !s0
  | j < n, let c = at bs j, c >= 48, c <= 57 =
      if nd >= 18 then rdSlow bs neg s0
      else let nd' = if m == 0 && c == 48 then nd else nd + 1
           in rdFrac bs (j + 1) (m * 10 + fromIntegral (c - 48)) nd' (any_ + 1) (fd + 1) neg s0
  | otherwise = rdExpo bs j m any_ fd neg
  where n = B.length bs

rdExpo :: B.ByteString -> Int -> Int -> Int -> Int -> Bool -> (# (# #) | Double# #)
rdExpo bs !j !m !any_ !fd !neg
  | any_ == 0 = (# (# #) | #)
  | j < n, let c = at bs j, c == 101 || c == 69 || c == 100 || c == 68 =
      if j + 1 >= n then (# (# #) | #) else
      let c1 = at bs (j + 1)
      in if c1 == 45 then rdExpDigits bs (j + 2) 0 0 True m fd neg
         else if c1 == 43 then rdExpDigits bs (j + 2) 0 0 False m fd neg
         else rdExpDigits bs (j + 1) 0 0 False m fd neg
  | otherwise = rdFin bs j m (negate fd) neg
  where n = B.length bs

rdExpDigits :: B.ByteString -> Int -> Int -> Int -> Bool -> Int -> Int -> Bool -> (# (# #) | Double# #)
rdExpDigits bs !k !e !cnt !eneg !m !fd !neg
  | k < n, let c = at bs k, c >= 48, c <= 57 = rdExpDigits bs (k + 1) (min 100000 (e * 10 + c - 48)) (cnt + 1) eneg m fd neg
  | cnt == 0 = (# (# #) | #)
  | otherwise = rdFin bs k m ((if eneg then negate e else e) - fd) neg
  where n = B.length bs

rdFin :: B.ByteString -> Int -> Int -> Int -> Bool -> (# (# #) | Double# #)
rdFin bs !j !m !e !neg
  | not (blanks j) = (# (# #) | #)
  | otherwise = case (if neg then negate (build m e) else build m e) of D# d -> (# | d #)
  where
    n = B.length bs
    blanks !i = i >= n || (isSp (at bs i) && blanks (i + 1))

-- the mantissa has at most 18 digits, so it is a positive 'Int'
build :: Int -> Int -> Double
build m e
  | m == 0 = 0
  | m < 9007199254740992 && e >= 0 && e <= 22 = fromIntegral m * pow10 e
  | m < 9007199254740992 && e < 0 && e >= -22 = fromIntegral m / pow10 (negate e)
  | otherwise = buildSlow m e
{-# INLINE build #-}

buildSlow :: Int -> Int -> Double
buildSlow m e = case eiselLemire (fromIntegral m) e of
  Just d -> d
  Nothing -> exact (toInteger m) e
{-# NOINLINE buildSlow #-}

-- too many digits to hold in a Int: parse with Integer
rdSlow :: B.ByteString -> Bool -> Int -> (# (# #) | Double# #)
rdSlow bs neg s0 = case exactParse bs s0 of
  Just d -> case (if neg then negate d else d) of D# x -> (# | x #)
  Nothing -> (# (# #) | #)

exact :: Integer -> Int -> Double
exact m e
  | e > 400 = 1 / 0
  | e < -800 = 0
  | e >= 0 = fromRational ((m * 10 ^ e) % 1)
  | otherwise = fromRational (m % (10 ^ negate e))

-- | Slow, exact path for numbers with more digits than a 'Word64' holds.
-- Starts at the sign (if any) of the number.
exactParse :: B.ByteString -> Int -> Maybe Double
exactParse bs s0 = sign s0
  where
    n = B.length bs
    sign i | i < n, at bs i == 45 || at bs i == 43 = ints (i + 1) 0 (0 :: Int)
           | otherwise = ints i 0 0
    ints !i !m !cnt
      | i < n, let c = at bs i, c >= 48, c <= 57 = ints (i + 1) (m * 10 + toInteger (c - 48)) (cnt + 1)
      | i < n, at bs i == 46 = frac (i + 1) m cnt 0
      | otherwise = ex i m cnt 0
    frac !i !m !cnt !fd
      | i < n, let c = at bs i, c >= 48, c <= 57 = frac (i + 1) (m * 10 + toInteger (c - 48)) (cnt + 1) (fd + 1)
      | otherwise = ex i m cnt fd
    ex !i !m !cnt !fd
      | cnt == 0 = Nothing
      | i < n, let c = at bs i, c == 101 || c == 69 || c == 100 || c == 68 =
          let (eneg, j) | i + 1 < n && at bs (i + 1) == 45 = (True, i + 2)
                        | i + 1 < n && at bs (i + 1) == 43 = (False, i + 2)
                        | otherwise = (False, i + 1)
          in expo j eneg 0 (0 :: Int) m fd
      | all (isSp . fromIntegral) (B.unpack (B.drop i bs)) = Just (exact m (negate fd))
      | otherwise = Nothing
    expo !k !eneg !e !cnt m fd
      | k < n, let c = at bs k, c >= 48, c <= 57 = expo (k + 1) eneg (min 100000 (e * 10 + c - 48)) (cnt + 1) m fd
      | cnt == 0 = Nothing
      | all (isSp . fromIntegral) (B.unpack (B.drop k bs)) = Just (exact m ((if eneg then negate e else e) - fd))
      | otherwise = Nothing
