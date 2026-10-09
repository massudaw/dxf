-- | Doubles written as text: every one reads back to itself, and the
-- digits are the shortest ones (all but a handful).
module Check.Doubles (checks) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (isDigit)
import Data.Word (Word64)
import GHC.Float (castWord64ToDouble)
import Check.Util
import DXF

text :: Double -> B.ByteString
text = BL.toStrict . BB.toLazyByteString . doubleText

rands :: Word64 -> [Word64]
rands = tail . iterate (\x -> let a = x `xor` (x `shiftL` 13); b = a `xor` (a `shiftR` 7) in b `xor` (b `shiftL` 17))

-- | Doubles of every exponent in the range the formatter handles, with
-- random mantissas; short decimals; integers; the neighbours of powers of ten.
samplesD :: [Double]
samplesD = concat
  [ [ castWord64ToDouble ((r .&. 0x800fffffffffffff) .|. (fromIntegral (1023 + e) `shiftL` 52))
    | (r, i) <- zip (rands 88172645463325252) [0 .. 200000 :: Int], let e = i `mod` 78 - 16 ]
  , [ fromIntegral k / 100 | k <- [-3000 .. 3000 :: Int] ]
  , [ fromIntegral k / 1000000 | k <- [0, 7 .. 700000 :: Int] ]
  , [ fromIntegral k | k <- [-1000 .. 1000 :: Int] ]
  , [ x | p <- [-5 .. 15 :: Int], let t = 10 ^^ p :: Double, x <- [t, t * (1 + 2.2e-16), t * (1 - 1.1e-16)] ]
  , [0, -0.0, 1e-5, 9.999999999999999e15, 0.1, 0.30000000000000004, 123456789012345678, 5e-324, 1e100, 1e-100, -2.5e-7] ]

-- | Significant digits of a decimal text.
sig :: B.ByteString -> Int
sig s = length (trim (reverse (trim (reverse ds))))
  where ds = filter isDigit (takeWhile (/= 'e') (BC.unpack s))
        trim = dropWhile (== '0')

checks :: [(String, IO Result)]
checks =
  [ ("doubles: text reads back to the same double", pure (concat
      [ failIf (readDouble t /= Just x && not (isNaN x)) ("doubleText " ++ show x ++ " = " ++ show t)
      | x <- samplesD, let t = text x ]))
  , ("doubles: text is plain decimal in range, and shortest but for a few", pure (
      let inRange = [x | x <- samplesD, abs x >= 1e-5, abs x < 1e16]
          longer = [x | x <- inRange, sig (text x) > sig (BL.toStrict (BB.toLazyByteString (BB.doubleDec x)))]
      in concat [ [ "e in " ++ show x ++ ": " ++ show (text x) | x <- inRange, BC.elem 'e' (text x) ]
                , failIf (length longer * 200 > length inRange) ("too many longer than shortest: " ++ show (length longer) ++ " of " ++ show (length inRange) ++ ", e.g. " ++ show (take 3 longer)) ])) ]
