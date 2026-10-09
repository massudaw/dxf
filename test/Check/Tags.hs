{-# LANGUAGE OverloadedStrings #-}
-- | The tag layer: numbers, text round trip, binary round trip.
module Check.Tags (checks) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Bits (shiftR, xor, shiftL)
import Data.Word (Word64)
import Numeric (showEFloat)
import Check.Util
import DXF

strictB :: BB.Builder -> B.ByteString
strictB = BL.toStrict . BB.toLazyByteString

-- xorshift, to make numbers without a library
rands :: Word64 -> [Word64]
rands = tail . iterate (\x -> let a = x `xor` (x `shiftL` 13); b = a `xor` (a `shiftR` 7) in b `xor` (b `shiftL` 17))

numbers :: Result
numbers = concat $
  [ failIf (readDouble s /= Just (read (norm (BC.unpack s)))) ("readDouble " ++ show s)
  | s <- fixed ++ generated ]
  ++ [ failIf (readInt "  42 " /= Just 42) "readInt 42", failIf (readInt "-7" /= Just (-7)) "readInt -7"
     , failIf (readInt "12.0" /= Just 12) "readInt 12.0", failIf (readInt "x" /= Nothing) "readInt x"
     , failIf (readDouble "" /= Nothing) "readDouble empty", failIf (readDouble "1e" /= Nothing) "readDouble 1e"
     , failIf (readDouble "." /= Nothing) "readDouble .", failIf (readHex "1F" /= Just 31) "readHex" ]
  where
    norm = filter (\c -> c /= '+' && c /= ' ')
    fixed = [ "0", "0.0", "-0.0", "1", "1.5", "  2.25  ", "420.0", "1e+20", "-1e+20", "1.0e-2", "0.1", "0.30000000000000004"
            , "123456789012345678", "1234567890123456789012345", "0.000000000000000000000001234"
            , "9007199254740993", "2.2250738585072014e-308", "1.7976931348623157e308", "5e-324", "4.9406564584124654e-324"
            , "3.141592653589793238462643383279", "100000000000000000000000", "0.5E3", "7e0" ]
    generated = [ BC.pack (showEFloat Nothing (d w) "") | w <- take 300 (rands 88172645463325252) ]
              ++ [ BC.pack (show (d w)) | w <- take 300 (rands 1234567) ]
    d w = (fromIntegral (w `shiftR` 11) / 9007199254740992) * 10 ^^ (fromIntegral (w `mod` 40) - 20 :: Int) :: Double

roundTrips :: IO Result
roundTrips = do
  fs <- sampleFiles
  concat <$> mapM (\f -> attempt f (one f)) fs
  where
    one f = do
      bs <- B.readFile f
      let (fmt, ts) = scanTags bs
          out = strictB (tagsFile fmt ts)
          (fmt', ts') = scanTags out
          bin = strictB (tagsFile fmt { fmtBinary = True, fmtR12 = isR12 ts } ts)
          (fb, tb) = scanTags bin
      return $ concat
        [ failIf (fmt /= fmt') (f ++ ": format changed")
        , failIf (ts /= ts') (f ++ ": tags differ after print")
        , failIf (out /= bs) (f ++ ": text is not byte for byte the input")
        , failIf (not (fmtBinary fb)) (f ++ ": binary not detected")
        , failIf (length ts /= length tb || not (and (zipWith tagEqv ts tb))) (f ++ ": binary round trip differs")
        , failIf (fmtR12 fb /= isR12 ts) (f ++ ": binary R12 flag") ]
    isR12 ts = case [v | (Tag 9 "$ACADVER", Tag 1 v) <- zip ts (drop 1 ts)] of
      (v : _) -> v <= "AC1009"
      [] -> True

-- | Binary files written by ezdxf against the text files it wrote from the same document.
ezdxfBinary :: IO Result
ezdxfBinary = concat <$> mapM one ["r12", "r2007", "r2018"]
  where
    one n = attempt n $ do
      (fb, tb) <- scanTags <$> B.readFile ("test/data/" ++ n ++ ".bin.dxf")
      (ft, tt) <- scanTags <$> B.readFile ("test/data/" ++ n ++ ".txt.dxf")
      let tt' = filter (\(Tag c _) -> c /= 999) tt
          stamp (Tag _ v) = "1.4.4 @" `B.isPrefixOf` v || "{" `B.isPrefixOf` v  -- time stamps and GUIDs differ per save
          bad = [(a, b) | (a, b) <- zip tb tt', not (tagEqv a b), not (stamp a)]
      return $ concat
        [ failIf (not (fmtBinary fb) || fmtBinary ft) (n ++ ": format detection")
        , failIf (length tb /= length tt') (n ++ ": tag count " ++ show (length tb, length tt'))
        , [n ++ ": differ: " ++ show x | x <- take 3 bad] ]

checks :: [(String, IO Result)]
checks =
  [ ("numbers", return numbers)
  , ("tags: text and binary round trip of every sample", roundTrips)
  , ("tags: binary files written by ezdxf", ezdxfBinary)
  ]
