-- | Writing tags.  An 'Out' is a piece of output that knows how to write
-- itself in the layout it is given (text with LF or CR LF, padded group
-- codes or not, binary, R12 or later), so entities are described once.
module DXF.Tag.Out
  ( Out, runOut
  , tS, tD, tI, tB, tRaw, tTags
  , tV3, tV2, tV3z, tagsFile
  , tMarker, tIfR2000, tIfR12
  , tMaybeS, tMaybeD, tMaybeI
  , eol, doubleText
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import DXF.Tag
import DXF.Tag.Binary
import DXF.Tag.Double
import DXF.Vec

type Out = Fmt -> BB.Builder

runOut :: Fmt -> Out -> BB.Builder
runOut f o = o f

eol :: Fmt -> BB.Builder
eol f = if fmtCRLF f then BB.word16LE 0x0A0D else BB.word8 10
{-# INLINE eol #-}

code :: Fmt -> Int -> BB.Builder
code f c
  | fmtPad f && c >= 0 && c < 100 = BB.char7 ' ' <> (if c < 10 then BB.char7 ' ' else mempty) <> BB.intDec c
  | otherwise = BB.intDec c
{-# INLINE code #-}

txt :: Fmt -> Int -> BB.Builder -> BB.Builder
txt f c v = code f c <> eol f <> v <> eol f
{-# INLINE txt #-}

-- | A string value, bytes as they are.
tS :: Int -> B.ByteString -> Out
tS c s f
  | fmtBinary f = binS (fmtR12 f) c s
  | otherwise = txt f c (BB.byteString s)

tD :: Int -> Double -> Out
tD c d f
  | fmtBinary f = binD (fmtR12 f) c d
  | otherwise = txt f c (doubleText d)

tI :: Int -> Int -> Out
tI c i f
  | fmtBinary f = binI (fmtR12 f) c i
  | otherwise = txt f c (BB.intDec i)

-- | A boolean (0 or 1).
tB :: Int -> Bool -> Out
tB c b = tI c (if b then 1 else 0)

-- | A tag as it was read.
tRaw :: Tag -> Out
tRaw t@(Tag c v) f
  | fmtBinary f = binaryTag (fmtR12 f) t
  | otherwise = txt f c (BB.byteString v)

-- | A whole file of tags, with the signature a binary file starts with.
tagsFile :: Fmt -> [Tag] -> BB.Builder
tagsFile f ts = (if fmtBinary f then BB.byteString binarySignature else mempty) <> tTags ts f

tTags :: [Tag] -> Out
tTags ts f = foldMap (\t -> tRaw t f) ts

-- | x, y, z at group codes c, c+10, c+20.
tV3 :: Int -> V3 -> Out
tV3 c (V3 x y z) = tD c x <> tD (c + 10) y <> tD (c + 20) z

-- | x and y only.
tV2 :: Int -> V2 -> Out
tV2 c (V2 x y) = tD c x <> tD (c + 10) y

-- | A 3D point whose z is left out when it is 0 (R12 style 2D data).
tV3z :: Int -> V3 -> Out
tV3z c (V3 x y z) = tD c x <> tD (c + 10) y <> (if z == 0 then mempty else tD (c + 20) z)

tMaybeS :: Int -> Maybe B.ByteString -> Out
tMaybeS c = maybe mempty (tS c)

tMaybeD :: Int -> Maybe Double -> Out
tMaybeD c = maybe mempty (tD c)

tMaybeI :: Int -> Maybe Int -> Out
tMaybeI c = maybe mempty (tI c)

-- | A subclass marker (@100@), which R12 does not have.
tMarker :: B.ByteString -> Out
tMarker name = tIfR2000 (tS 100 name)

-- | Output that only exists after R12.
tIfR2000 :: Out -> Out
tIfR2000 o f = if fmtR12 f then mempty else o f

-- | Output that only exists in R12 (and older).
tIfR12 :: Out -> Out
tIfR12 o f = if fmtR12 f then o f else mempty
