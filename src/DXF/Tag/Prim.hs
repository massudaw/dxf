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
import Data.ByteString.Builder.Prim.Internal (runB)
import Data.Word
import Foreign.Ptr (Ptr)
import LinBuf.Fixed (Sized (..))
import qualified LinBuf.Fixed as F
import LinBuf.Builder (asBoundedPrim)
import DXF.Tag.Double (real)

-- A tag is one linbuf 'F.Bounded' (code line, value, line end), its size in its type, run at the
-- builder's address ('asBoundedPrim'): the layout's choices (padding, line end, a zero) are 'F.choose'
-- between stores widened ('F.widen') to one size.  No address is computed here.

-- | Padding of a code to three columns, if the file pads: two spaces, one, or none.
padB :: Bool -> Int -> F.Bounded 2
padB pad c = F.choose (pad && c >= 0 && c < 10) (F.bounded (F.u16le 0x2020))
               (F.choose (pad && c >= 10 && c < 100) (F.widen (F.bounded (F.u8 32))) (F.widen none))
{-# INLINE padB #-}

-- | A line end: CR LF or LF.
eolB :: Bool -> F.Bounded 2
eolB crlf = F.choose crlf (F.bounded (F.u16le 0x0A0D)) (F.widen (F.bounded (F.u8 10)))
{-# INLINE eolB #-}

-- | The group code line: the code, padded to three columns if the file is, and its line end.
codeB :: Bool -> Bool -> Int -> F.Bounded 24
codeB pad crlf c = padB pad c <+> F.intDec c <+> eolB crlf
{-# INLINE codeB #-}

intTagB :: Bool -> Bool -> Int -> Int -> F.Bounded 46
intTagB pad crlf c i = codeB pad crlf c <+> F.intDec i <+> eolB crlf
{-# INLINE intTagB #-}

doubleTagB :: Bool -> Bool -> Int -> Double -> F.Bounded 68
doubleTagB pad crlf c x =
  codeB pad crlf c <+> F.choose (x == 0) (F.widen (F.bounded zero)) (real x) <+> eolB crlf
  where zero = F.u16le 0x2E30 <+> F.u8 48     -- "0.0"
{-# INLINE doubleTagB #-}

bytesTagB :: Bool -> Bool -> Int -> B.ByteString -> F.Bounded 90
bytesTagB pad crlf c v = codeB pad crlf c <+> F.bytesUpTo @64 v <+> eolB crlf
{-# INLINE bytesTagB #-}

-- | Whether a double is written by the primitive (plain decimal).
plainDouble :: Double -> Bool
plainDouble d = (a >= 1e-5 && a < 1e16) || (d == 0 && not (isNegativeZero d))
  where a = abs d
{-# INLINE plainDouble #-}

-- | Whether bytes are written by the primitive.
shortBytes :: B.ByteString -> Bool
shortBytes s = B.length s <= 64
{-# INLINE shortBytes #-}

-- | The writers behind the three tag builders, for callers that write several tags in one primitive.  A
-- double must satisfy 'plainDouble', bytes 'shortBytes'.  What one writes, for a code of at most four
-- digits (DXF's are): a double at most 50 bytes, bytes 72, an int 28 -- within the 96, 80 and 48 the
-- callers reserve.
putDoubleTag :: Bool -> Bool -> Int -> Double -> Ptr Word8 -> IO (Ptr Word8)
putDoubleTag pad crlf c x p = runB (asBoundedPrim (doubleTagB pad crlf c)) x p
{-# NOINLINE putDoubleTag #-}

putIntTag :: Bool -> Bool -> Int -> Int -> Ptr Word8 -> IO (Ptr Word8)
putIntTag pad crlf c i p = runB (asBoundedPrim (intTagB pad crlf c)) i p
{-# NOINLINE putIntTag #-}

putBytesTag :: Bool -> Bool -> Int -> B.ByteString -> Ptr Word8 -> IO (Ptr Word8)
putBytesTag pad crlf c v p = runB (asBoundedPrim (bytesTagB pad crlf c)) v p
{-# NOINLINE putBytesTag #-}

-- | A tag whose value is a double.
doubleTag :: Bool -> Bool -> Int -> Double -> BB.Builder -> BB.Builder
doubleTag pad crlf c d fallback
  | plainDouble d = primBounded (asBoundedPrim (doubleTagB pad crlf c)) d
  | otherwise = fallback

-- | A tag whose value is an integer.
intTag :: Bool -> Bool -> Int -> Int -> BB.Builder
intTag pad crlf c = primBounded (asBoundedPrim (intTagB pad crlf c))

-- | A tag whose value is bytes (a short string is written by the
-- primitive; a long one by the fallback).
bytesTag :: Bool -> Bool -> Int -> B.ByteString -> BB.Builder -> BB.Builder
bytesTag pad crlf c s fallback
  | shortBytes s = primBounded (asBoundedPrim (bytesTagB pad crlf c)) s
  | otherwise = fallback
