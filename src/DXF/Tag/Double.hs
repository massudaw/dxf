{-# LANGUAGE DataKinds, OverloadedStrings #-}
-- | Doubles as text: linbuf's shortest digits ('LinBuf.Real.shortestDigits': Schubfach -- the fewest digits
-- that read back to the same double, the closest of those, ties to even), laid out as dxf writes them, in
-- plain decimal notation (@0.01@, @250.24323977301532@, @3.0@), as CAD programs write it; outside
-- 1e-5 .. 1e16 the general formatter is used.
--
-- Each layout is one linbuf 'F.Bounded' run in the output buffer through the Builder bridge
-- ('asBoundedPrim'); which layout (a sign or none, below 1e-4 or not) is linbuf's 'F.choose', the smaller
-- layouts widened ('F.widen') to the size of the larger.
module DXF.Tag.Double
  ( doubleText, writeDouble, realPrim
  ) where

import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder.Prim (BoundedPrim, primBounded)
import Data.ByteString.Builder.Prim.Internal (runB)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import LinBuf.Builder (asBoundedPrim)
import LinBuf.Fixed (Sized (..))
import qualified LinBuf.Fixed as F
import LinBuf.Real (shortestDigits, wordDecPadded)

doubleText :: Double -> BB.Builder
doubleText d
  | a >= 1e-5 && a < 1e16 = primBounded realPrim d
  | d == 0 = if isNegativeZero d then "-0.0" else "0.0"
  | otherwise = BB.doubleDec d
  where a = abs d
{-# INLINE doubleText #-}

-- | Sign, digits and the point, for a double of magnitude in 1e-5 .. 1e16 (at most 42 bytes).
writeDouble :: Double -> Ptr Word8 -> IO (Ptr Word8)
writeDouble = runB realPrim
{-# INLINE writeDouble #-}

realPrim :: BoundedPrim Double
realPrim = asBoundedPrim real

-- | A sign or none, then the magnitude.
real :: Double -> F.Bounded 42
real x = F.choose (x < 0) (F.bounded (F.u8 45) <+> magnitude (negate x)) (F.widen (magnitude x))
{-# INLINE real #-}

magnitude :: Double -> F.Bounded 41
magnitude a = F.choose (a < 1e-4) (F.widen (below a)) (plain a)
{-# INLINE magnitude #-}

-- | A magnitude of at least 1e-4: the integer part, the point, the fraction (at least one digit; at most 20,
-- as the shortest digits are at most 17 and the point at most three places before them).
plain :: Double -> F.Bounded 41
plain x = F.wordDec (fromIntegral i) <+> F.bounded (F.u8 46) <+> wordDecPadded l (fromIntegral f)
  where
    (m, _, e) = shortestDigits x
    l = if e >= 0 then 1 else negate e
    (i, f) | e >= 0 = (m * p10 e, 0)
           | l < 20 = m `quotRem` p10 l
           | otherwise = (0, m)
{-# INLINE plain #-}

-- | A magnitude in 1e-5 .. 1e-4: its first digit is the fifth after the point (the shortest digits of a double
-- below the double 1e-4 are below 1e-4, those of one at or above the double 1e-5 at or above 1e-5).
below :: Double -> F.Bounded 26
below x = F.bounded zeros4 <+> F.wordDec (fromIntegral m)
  where (m, _, _) = shortestDigits x
{-# INLINE below #-}

zeros4 :: F.Fixed 6
zeros4 = F.u8 48 <+> F.u8 46 <+> F.u8 48 <+> F.u8 48 <+> F.u8 48 <+> F.u8 48
{-# INLINE zeros4 #-}

-- | 10^k, for k in 0 .. 19.
p10 :: Int -> Word64
p10 k = case k of
  0 -> 1; 1 -> 10; 2 -> 100; 3 -> 1000; 4 -> 10000; 5 -> 100000; 6 -> 1000000; 7 -> 10000000
  8 -> 100000000; 9 -> 1000000000; 10 -> 10000000000; 11 -> 100000000000; 12 -> 1000000000000
  13 -> 10000000000000; 14 -> 100000000000000; 15 -> 1000000000000000; 16 -> 10000000000000000
  17 -> 100000000000000000; 18 -> 1000000000000000000; _ -> 10000000000000000000
{-# INLINE p10 #-}
