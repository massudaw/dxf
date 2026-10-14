{-# LANGUAGE BangPatterns, DataKinds, TypeApplications #-}
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
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, FixedPrim, boundedPrim, runB, runF)
import Data.Word
import Foreign.Ptr
import qualified LinBuf.Fixed as F
import LinBuf.Builder (asBoundedPrim, asFixedPrim)
import DXF.Tag.Double (writeDouble)

-- The stores are linbuf's ('LinBuf.Fixed'), run at the builder's address
-- ('asFixedPrim', 'asBoundedPrim', then 'runF'/'runB'): each of a size in
-- its type, within the bound of the primitive that runs them all.  What is
-- left here is the layout -- which store, and where the next one begins.

fixedAt :: FixedPrim () -> Int -> Ptr Word8 -> IO (Ptr Word8)
fixedAt fp k p = runF fp () p >> pure (p `plusPtr` k)
{-# INLINE fixedAt #-}

lfP, crlfP, sp1P, sp2P, zeroP :: FixedPrim ()
lfP = asFixedPrim (\() -> F.u8 10)
crlfP = asFixedPrim (\() -> F.u16le 0x0A0D)          -- CR, LF
sp1P = asFixedPrim (\() -> F.u8 32)
sp2P = asFixedPrim (\() -> F.u16le 0x2020)
zeroP = asFixedPrim (\() -> F.u16le 0x2E30 F.<+> F.u8 48)  -- "0.0"
{-# INLINE lfP #-}
{-# INLINE crlfP #-}
{-# INLINE sp1P #-}
{-# INLINE sp2P #-}
{-# INLINE zeroP #-}

intP :: BoundedPrim Int
intP = asBoundedPrim F.intDec
{-# INLINE intP #-}

eolW :: Bool -> Ptr Word8 -> IO (Ptr Word8)
eolW crlf
  | crlf = fixedAt crlfP 2
  | otherwise = fixedAt lfP 1
{-# INLINE eolW #-}

putInt :: Int -> Ptr Word8 -> IO (Ptr Word8)
putInt = runB intP
{-# INLINE putInt #-}

-- | A short string's bytes ('shortBytes': at most 64), one copy.
bytesP :: BoundedPrim B.ByteString
bytesP = asBoundedPrim (F.bytesUpTo @64)
{-# INLINE bytesP #-}

-- | The group code line: the code, padded to three columns if the file
-- is, and its line end.
codeLine :: Bool -> Bool -> Int -> Ptr Word8 -> IO (Ptr Word8)
codeLine pad crlf c p0 = do
  p1 <- if pad && c >= 0 && c < 100
          then if c < 10 then fixedAt sp2P 2 p0 else fixedAt sp1P 1 p0
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
  p2 <- if x == 0 then fixedAt zeroP 3 p1 else writeDouble x p1
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
  p2 <- runB bytesP v p1
  eolW crlf p2

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
