{-# LANGUAGE BangPatterns #-}
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
import DXF.Tag.Prim
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
  | otherwise = bytesTag (fmtPad f) (fmtCRLF f) c s (txt f c (BB.byteString s))
{-# INLINE tS #-}

tD :: Int -> Double -> Out
tD c d f
  | fmtBinary f = binD (fmtR12 f) c d
  | otherwise = doubleTag (fmtPad f) (fmtCRLF f) c d (txt f c (doubleText d))
{-# INLINE tD #-}

tI :: Int -> Int -> Out
tI c i f
  | fmtBinary f = binI (fmtR12 f) c i
  | otherwise = intTag (fmtPad f) (fmtCRLF f) c i
{-# INLINE tI #-}

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
tV3 !c (V3 x y z) f = let !c1 = c + 10; !c2 = c + 20 in tD c x f <> tD c1 y f <> tD c2 z f
{-# INLINE tV3 #-}

-- | x and y only.
tV2 :: Int -> V2 -> Out
tV2 !c (V2 x y) = let !c1 = c + 10 in tD c x <> tD c1 y

-- | A 3D point whose z is left out when it is 0 (R12 style 2D data).
tV3z :: Int -> V3 -> Out
tV3z !c (V3 x y z) = let !c1 = c + 10; !c2 = c + 20 in tD c x <> tD c1 y <> (if z == 0 then mempty else tD c2 z)

tMaybeS :: Int -> Maybe B.ByteString -> Out
tMaybeS c m f = case m of Nothing -> mempty; Just s -> tS c s f
{-# INLINE tMaybeS #-}

tMaybeD :: Int -> Maybe Double -> Out
tMaybeD c m f = case m of Nothing -> mempty; Just d -> tD c d f
{-# INLINE tMaybeD #-}

tMaybeI :: Int -> Maybe Int -> Out
tMaybeI c m f = case m of Nothing -> mempty; Just i -> tI c i f
{-# INLINE tMaybeI #-}

-- | A subclass marker (@100@), which R12 does not have.
tMarker :: B.ByteString -> Out
tMarker name f = if fmtR12 f then mempty else tS 100 name f
{-# INLINE tMarker #-}

-- | Output that only exists after R12.
tIfR2000 :: Out -> Out
tIfR2000 o f = if fmtR12 f then mempty else o f

-- | Output that only exists in R12 (and older).
tIfR12 :: Out -> Out
tIfR12 o f = if fmtR12 f then o f else mempty
