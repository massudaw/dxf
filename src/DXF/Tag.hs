-- | The tag layer: a DXF file is a sequence of (group code, value) pairs.
--
-- A 'Tag' keeps its value as the bytes it was read from -- a slice of the
-- input, not a copy -- in the file's text form, whether the file was text
-- or binary.  Numbers are parsed from those bytes when something asks.
module DXF.Tag
  ( Tag (..), Kind (..), kindOf
  , tagInt, tagDouble, tagStr, tagEqv
  , isPointCode, isBinaryCode
  , Fmt (..), defaultFmt
  , DxfError (..)
  ) where

import Control.Exception (Exception)
import qualified Data.ByteString as B
import Control.DeepSeq (NFData (..))
import DXF.Number

data Tag = Tag {-# UNPACK #-} !Int {-# UNPACK #-} !B.ByteString
  deriving (Eq, Ord, Show)

instance NFData Tag where rnf (Tag _ _) = ()

-- | How a group code's value is typed (as ezdxf's tag type table, which
-- is also the layout of binary DXF).
data Kind = KStr | KDouble | KInt16 | KInt32 | KInt64 | KBool | KBin
  deriving (Eq, Show)

kindOf :: Int -> Kind
kindOf c
  | c < 10 = KStr
  | c < 60 = KDouble
  | c < 80 = KInt16
  | c < 90 = KStr
  | c < 100 = KInt32
  | c < 110 = KStr
  | c < 150 = KDouble
  | c < 160 = KStr
  | c < 170 = KInt64
  | c < 180 = KInt16
  | c < 210 = KStr
  | c < 240 = KDouble
  | c < 270 = KStr
  | c < 290 = KInt16
  | c < 300 = KBool
  | c < 310 = KStr
  | c < 320 = KBin
  | c < 370 = KStr
  | c < 390 = KInt16
  | c < 400 = KStr
  | c < 410 = KInt16
  | c < 420 = KStr
  | c < 430 = KInt32
  | c < 440 = KStr
  | c < 460 = KInt32
  | c < 470 = KDouble
  | c < 1004 = KStr
  | c == 1004 = KBin
  | c < 1010 = KStr
  | c < 1060 = KDouble
  | c < 1071 = KInt16
  | c == 1071 = KInt32
  | otherwise = KStr

-- | Group codes whose x, y, z come as c, c+10, c+20.
isPointCode :: Int -> Bool
isPointCode c = (c >= 10 && c <= 18) || (c >= 110 && c <= 112) || (c >= 210 && c <= 213) || (c >= 1010 && c <= 1013)

isBinaryCode :: Int -> Bool
isBinaryCode c = (c >= 310 && c <= 319) || c == 1004

tagInt :: Tag -> Int
tagInt (Tag _ v) = maybe 0 id (readInt v)

tagDouble :: Tag -> Double
tagDouble (Tag _ v) = maybe 0 id (readDouble v)

tagStr :: Tag -> B.ByteString
tagStr (Tag _ v) = v

-- | Same group code, same value: numbers by what they are (@1.50@ and
-- @1.5@ are one value), everything else by its bytes.
tagEqv :: Tag -> Tag -> Bool
tagEqv a@(Tag c v) b@(Tag c' v')
  | c /= c' = False
  | v == v' = True
  | otherwise = case kindOf c of
      KDouble -> readDouble v == readDouble v' && readDouble v /= Nothing
      KStr -> False
      KBin -> False
      _ -> tagInt a == tagInt b && readInt v /= Nothing

-- | How a file is laid out, so that it can be written the same way.
data Fmt = Fmt
  { fmtBinary :: !Bool  -- ^ binary DXF
  , fmtCRLF   :: !Bool  -- ^ text lines end in CR LF
  , fmtPad    :: !Bool  -- ^ group codes right-aligned in 3 columns
  , fmtR12    :: !Bool  -- ^ R12 or older: no subclass markers, 1-byte binary codes
  } deriving (Eq, Show)

defaultFmt :: Fmt
defaultFmt = Fmt False False True False

data DxfError = DxfError !Int String
  deriving Show

instance Exception DxfError
