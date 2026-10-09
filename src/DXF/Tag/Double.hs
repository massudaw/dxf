{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Doubles as text, fast: Grisu2 (Loitsch 2010), written into the output
-- buffer with no allocation.  The digits always read back to the same
-- double ('readDouble' is correctly rounded) and are the shortest ones
-- in all but about one case in a thousand.  Plain decimal notation
-- (@0.01@, @250.24323977301532@, @3.0@), as CAD programs write it;
-- outside 1e-5 .. 1e16 the general formatter is used.
module DXF.Tag.Double
  ( doubleText
  ) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.Prim.Internal (boundedPrim)
import Data.Word
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr
import Foreign.Storable
import GHC.Float (castDoubleToWord64)

doubleText :: Double -> BB.Builder
doubleText d
  | a >= 1e-5 && a < 1e16 = primBounded (boundedPrim 64 write) d
  | d == 0 = if isNegativeZero d then "-0.0" else "0.0"
  | otherwise = BB.doubleDec d
  where a = abs d
{-# INLINE doubleText #-}

-- | 10^(-348 + 8 i) as a 64-bit significand and a binary exponent, for i in 0..86.
cachedF :: UArray Int Word64
cachedF = listArray (0, 86) (map fst cachedTable)

cachedE :: UArray Int Int
cachedE = listArray (0, 86) (map snd cachedTable)

cachedTable :: [(Word64, Int)]
cachedTable = [entry (-348 + 8 * i) | i <- [0 .. 86]]
  where
    entry :: Int -> (Word64, Int)
    entry p = norm (bl num - bl den - 64)
      where
        (num, den) = if p >= 0 then (10 ^ p, 1) else (1, 10 ^ negate p) :: (Integer, Integer)
        bl :: Integer -> Int
        bl = length . takeWhile (> 0) . iterate (`shiftR` 1)
        scaled e = if e >= 0 then rdiv num (den `shiftL` e) else rdiv (num `shiftL` negate e) den
        rdiv a b = (2 * a + b) `div` (2 * b)
        norm e = let x = scaled e
                 in if x >= 2 ^ (64 :: Int) then norm (e + 1)
                    else if x < 2 ^ (63 :: Int) then norm (e - 1)
                    else (fromInteger x, e)

pow10 :: UArray Int Word64
pow10 = listArray (0, 19) (iterate (* 10) 1)

-- | The high 64 bits of a product, rounded.
mulhi :: Word64 -> Word64 -> Word64
mulhi a b = ac + (ad `unsafeShiftR` 32) + (bc `unsafeShiftR` 32) + (tmp `unsafeShiftR` 32)
  where
    m = 0xffffffff
    ah = a `unsafeShiftR` 32; al = a .&. m
    bh = b `unsafeShiftR` 32; bl' = b .&. m
    ac = ah * bh; bc = al * bh; ad = ah * bl'; bd = al * bl'
    tmp = (bd `unsafeShiftR` 32) + (ad .&. m) + (bc .&. m) + 0x80000000

-- | Sign, digits and the point, for a double in range.
write :: Double -> Ptr Word8 -> IO (Ptr Word8)
write d p0 = do
  p <- if d < 0 then poke p0 45 >> pure (p0 `plusPtr` 1) else pure p0
  let bits = castDoubleToWord64 (abs d)
      ef = fromIntegral (bits `unsafeShiftR` 52) :: Int
      mant = bits .&. 0xfffffffffffff
      f = mant .|. 0x10000000000000
      e = ef - 1075
      -- boundaries of the rounding interval, the upper one normalised
      pf0 = f * 2 + 1
      s = countLeadingZeros pf0
      pf = pf0 `unsafeShiftL` s
      pe = e - 1 - s
      (mf0, me0) = if mant == 0 && ef > 1 then (f * 4 - 1, e - 2) else (f * 2 - 1, e - 1)
      mf = mf0 `unsafeShiftL` (me0 - pe)
      sv = countLeadingZeros f
      vf = f `unsafeShiftL` sv
      -- cached power of ten
      dk = fromIntegral (-61 - pe) * 0.30102999566398114 + 347 :: Double
      k0 = ceiling dk :: Int
      idx = (k0 `unsafeShiftR` 3) + 1
      kk = 348 - 8 * idx
      cf = unsafeAt cachedF idx
      ce = unsafeAt cachedE idx
      w = mulhi vf cf
      wp = mulhi pf cf - 1
      wm = mulhi mf cf + 1
      sh = negate (pe + ce + 64)
      buf = p `plusPtr` 32 :: Ptr Word8
  (n, k) <- digitGen buf w wp (wp - wm) sh kk
  let pos = n + k
  if k >= 0
    then do copyBytes p buf n
            fillBytes (p `plusPtr` n) 48 k
            pokeByteOff p pos (46 :: Word8)
            pokeByteOff p (pos + 1) (48 :: Word8)
            pure (p `plusPtr` (pos + 2))
    else if pos > 0
      then do copyBytes p buf pos
              pokeByteOff p pos (46 :: Word8)
              copyBytes (p `plusPtr` (pos + 1)) (buf `plusPtr` pos) (n - pos)
              pure (p `plusPtr` (n + 1))
      else do pokeByteOff p 0 (48 :: Word8)
              pokeByteOff p 1 (46 :: Word8)
              fillBytes (p `plusPtr` 2) 48 (negate pos)
              copyBytes (p `plusPtr` (2 - pos)) buf n
              pure (p `plusPtr` (2 - pos + n))

-- | The digits of the upper bound @mp@, as few as keep them in the interval
-- of width @delta@; (count, decimal exponent).
digitGen :: Ptr Word8 -> Word64 -> Word64 -> Word64 -> Int -> Int -> IO (Int, Int)
digitGen buf w mp delta0 sh k0 = intLoop p1 dv0 kappa0 0
  where
    one = 1 `unsafeShiftL` sh :: Word64
    mask = one - 1
    wpw = mp - w
    p1 = mp `unsafeShiftR` sh
    p2 = mp .&. mask
    kappa0 = length (takeWhile (<= p1) (map (unsafeAt pow10) [0 .. 19])) :: Int
    dv0 = if kappa0 == 0 then 1 else unsafeAt pow10 (kappa0 - 1)
    emit len v = if v /= 0 || len /= 0 then pokeByteOff buf len (48 + fromIntegral v :: Word8) >> pure (len + 1) else pure len
    intLoop !q !dv !kappa !len
      | kappa == 0 = fracLoop p2 delta0 0 len
      | otherwise = do
          let dgt = q `quot` dv
              r = q `rem` dv
              kappa' = kappa - 1
              tmp = (r `unsafeShiftL` sh) + p2
          len' <- emit len dgt
          if tmp <= delta0
            then roundDown len' delta0 tmp (dv `unsafeShiftL` sh) wpw >> pure (len', k0 + kappa')
            else intLoop r (dv `quot` 10) kappa' len'
    fracLoop !q !dl !kappa !len = do
      let q' = q * 10
          dl' = dl * 10
          dgt = q' `unsafeShiftR` sh
          q'' = q' .&. mask
          kappa' = kappa - 1 :: Int
      len' <- emit len dgt
      if q'' < dl'
        then roundDown len' dl' q'' one (wpw * unsafeAt pow10 (negate kappa')) >> pure (len', k0 + kappa')
        else fracLoop q'' dl' kappa' len'
    -- move the last digit closer to the true value while it stays inside
    roundDown len delta rest0 tenK wpw' = go rest0
      where
        go !rest
          | rest < wpw' && delta - rest >= tenK && (rest + tenK < wpw' || wpw' - rest > rest + tenK - wpw') = do
              c <- peekByteOff buf (len - 1) :: IO Word8
              pokeByteOff buf (len - 1) (c - 1)
              go (rest + tenK)
          | otherwise = pure ()
