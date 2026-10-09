{-# LANGUAGE BangPatterns #-}
-- | Whole text tags (code line, value line) written by one primitive each,
-- straight into the output buffer.  A 'Builder' pays for every piece it is
-- made of (code, line end, value, line end), so a file of a million tags
-- spends its time in calls between pieces; here a tag is one call.
--
-- Each writer takes the layout (padded codes, CR LF) and a builder to use
-- when the value does not fit the primitive's fixed bound or is not plain
-- decimal (huge, tiny or non-finite doubles, long strings).
module DXF.Tag.Prim
  ( doubleTag, intTag, bytesTag
  , putDoubleTag, putIntTag, putBytesTag, plainDouble, shortBytes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Unsafe as U
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.Prim.Internal (boundedPrim)
import Data.Word
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable
import DXF.Tag.Double (writeDouble)

eolW :: Bool -> Ptr Word8 -> IO (Ptr Word8)
eolW crlf p
  | crlf = pokeByteOff p 0 (13 :: Word8) >> pokeByteOff p 1 (10 :: Word8) >> pure (p `plusPtr` 2)
  | otherwise = pokeByteOff p 0 (10 :: Word8) >> pure (p `plusPtr` 1)
{-# INLINE eolW #-}

-- | A natural number in decimal.
putNat :: Word -> Ptr Word8 -> IO (Ptr Word8)
putNat w p = fill w (p `plusPtr` (k - 1)) >> pure (p `plusPtr` k)
  where
    k = len w 1
    len n !a = if n < 10 then a else len (n `quot` 10) (a + 1 :: Int)
    fill n q = do
      pokeByteOff q 0 (fromIntegral (48 + n `rem` 10) :: Word8)
      if n < 10 then pure () else fill (n `quot` 10) (q `plusPtr` (-1))

putInt :: Int -> Ptr Word8 -> IO (Ptr Word8)
putInt n p
  | n < 0 = pokeByteOff p 0 (45 :: Word8) >> putNat (fromIntegral (negate n)) (p `plusPtr` 1)
  | otherwise = putNat (fromIntegral n) p

-- | The group code line: the code, padded to three columns if the file
-- is, and its line end.
codeLine :: Bool -> Bool -> Int -> Ptr Word8 -> IO (Ptr Word8)
codeLine pad crlf c p0 = do
  p1 <- if pad && c >= 0 && c < 100
          then do pokeByteOff p0 0 (32 :: Word8)
                  if c < 10 then pokeByteOff p0 1 (32 :: Word8) >> pure (p0 `plusPtr` 2)
                            else pure (p0 `plusPtr` 1)
          else pure p0
  p2 <- putInt c p1
  eolW crlf p2

-- | Whether a double is written by the primitive (plain decimal).
plainDouble :: Double -> Bool
plainDouble d = (a >= 1e-5 && a < 1e16) || (d == 0 && not (isNegativeZero d))
  where a = abs d
{-# INLINE plainDouble #-}

-- | Whether bytes are written by the primitive.
shortBytes :: B.ByteString -> Bool
shortBytes s = B.length s <= 64
{-# INLINE shortBytes #-}

-- | The writers behind the three tag builders, for callers that write
-- several tags in one primitive.  A double must satisfy 'plainDouble'
-- (at most 96 bytes), bytes 'shortBytes' (at most 80), an int takes 48.
putDoubleTag :: Bool -> Bool -> Int -> Double -> Ptr Word8 -> IO (Ptr Word8)
putDoubleTag pad crlf c x p0 = do
  p1 <- codeLine pad crlf c p0
  p2 <- if x == 0 then do pokeByteOff p1 0 (48 :: Word8); pokeByteOff p1 1 (46 :: Word8)
                          pokeByteOff p1 2 (48 :: Word8); pure (p1 `plusPtr` 3)
        else writeDouble x p1
  eolW crlf p2
{-# INLINE putDoubleTag #-}

putIntTag :: Bool -> Bool -> Int -> Int -> Ptr Word8 -> IO (Ptr Word8)
putIntTag pad crlf c i p0 = do
  p1 <- codeLine pad crlf c p0
  p2 <- putInt i p1
  eolW crlf p2

putBytesTag :: Bool -> Bool -> Int -> B.ByteString -> Ptr Word8 -> IO (Ptr Word8)
putBytesTag pad crlf c v p0 = do
  p1 <- codeLine pad crlf c p0
  let n = B.length v
  U.unsafeUseAsCString v (\src -> copyBytes p1 (castPtr src) n)
  eolW crlf (p1 `plusPtr` n)

-- | A tag whose value is a double.
doubleTag :: Bool -> Bool -> Int -> Double -> BB.Builder -> BB.Builder
doubleTag pad crlf c d fallback
  | plainDouble d = primBounded (boundedPrim 96 (putDoubleTag pad crlf c)) d
  | otherwise = fallback

-- | A tag whose value is an integer.
intTag :: Bool -> Bool -> Int -> Int -> BB.Builder
intTag pad crlf c = primBounded (boundedPrim 48 (putIntTag pad crlf c))

-- | A tag whose value is bytes (a short string is written by the
-- primitive; a long one by the fallback).
bytesTag :: Bool -> Bool -> Int -> B.ByteString -> BB.Builder -> BB.Builder
bytesTag pad crlf c s fallback
  | shortBytes s = primBounded (boundedPrim 80 (putBytesTag pad crlf c)) s
  | otherwise = fallback
