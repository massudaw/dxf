{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | The usual entities written as one primitive each, the whole entity in
-- a single call into the output buffer: LINE, VERTEX, POLYLINE, SEQEND,
-- INSERT.  Anything unusual (application data, extended data, unplaced
-- tags, long strings, numbers outside plain decimal, binary output) is
-- left to the general writer, which says the same; the test suite compares
-- the two on every sample and fixture in every layout.
--
-- The children of an entity (a POLYLINE's vertices) are not written here.
module DXF.Entity.FastOut
  ( fastOut
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.Prim.Internal (boundedPrim)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import DXF.Entity.Common
import DXF.Entity.Geometry
import DXF.Entity.Poly
import DXF.Entity.Types
import DXF.Tag (Fmt (..))
import DXF.Tag.Prim
import DXF.Vec

-- | The entity (without its children) as one builder, or 'Nothing' when
-- the general writer must do it.
fastOut :: Fmt -> Entity -> Maybe BB.Builder
fastOut f (Entity ty c b _)
  | fmtBinary f || not (plainHead ty c) || not (null (cXData c)) || not (null (cExtra c)) = Nothing
  | otherwise = case b of
      BGeo (GLine (Line a e Nothing Nothing)) | ok3 a, ok3 e -> Just (prim 1700 (lineW w ty c a e))
      BPoly (PVertex v) | okVertex v -> Just (prim 2600 (vertexW w ty c v))
      BPoly (PPolyline p) | okPolyline p -> Just (prim 2600 (polylineW w ty c p))
      BPoly PSeqEnd -> Just (prim 1000 (\p -> headWriter pad crlf r12 True ty c p))
      BPoly (PInsert i) | okInsert i -> Just (prim 2600 (insertW w ty c i))
      _ -> Nothing
  where
    pad = fmtPad f
    crlf = fmtCRLF f
    r12 = fmtR12 f
    w = W pad crlf r12
    prim n act = primBounded (boundedPrim n (\() -> act)) ()

type BS = B.ByteString

-- | The layout.
data W = W !Bool !Bool !Bool

okD :: Maybe Double -> Bool
okD = maybe True plainDouble

ok3 :: V3 -> Bool
ok3 (V3 x y z) = plainDouble x && plainDouble y && plainDouble z

okM3 :: Maybe V3 -> Bool
okM3 = maybe True ok3

okVertex :: Vertex -> Bool
okVertex v = ok3 (vxLocation v) && all okD [vxStartWidth v, vxEndWidth v, vxBulge v, vxTangent v]

okPolyline :: Polyline -> Bool
okPolyline p = ok3 (plElevation p) && all okD [plStartWidth p, plEndWidth p, plThickness p] && okM3 (plExtrusion p)

okInsert :: Insert -> Bool
okInsert i = shortBytes (inName i) && ok3 (inInsert i)
  && all okD [inXScale i, inYScale i, inZScale i, inRotation i, inColSpace i, inRowSpace i] && okM3 (inExtrusion i)

type Put = Ptr Word8 -> IO (Ptr Word8)

pD :: W -> Int -> Double -> Put
pD (W pad crlf _) = putDoubleTag pad crlf
{-# INLINE pD #-}

pI :: W -> Int -> Int -> Put
pI (W pad crlf _) = putIntTag pad crlf
{-# INLINE pI #-}

pS :: W -> Int -> BS -> Put
pS (W pad crlf _) = putBytesTag pad crlf
{-# INLINE pS #-}

-- | An optional double / int.
oD :: W -> Int -> Maybe Double -> Put
oD w k m p = maybe (pure p) (\d -> pD w k d p) m
{-# INLINE oD #-}

oI :: W -> Int -> Maybe Int -> Put
oI w k m p = maybe (pure p) (\d -> pI w k d p) m
{-# INLINE oI #-}

-- | A point at codes k, k+10, k+20.
p3 :: W -> Int -> V3 -> Put
p3 w k (V3 x y z) p0 = pD w k x p0 >>= pD w (k + 10) y >>= pD w (k + 20) z
{-# INLINE p3 #-}

o3 :: W -> Int -> Maybe V3 -> Put
o3 w k m p = maybe (pure p) (\v -> p3 w k v p) m
{-# INLINE o3 #-}

-- | A subclass marker, but not in R12.
mk :: W -> BS -> Put
mk w@(W _ _ r12) m p = if r12 then pure p else pS w 100 m p
{-# INLINE mk #-}

hd :: W -> BS -> Common -> Put
hd (W pad crlf r12) ty c = headWriter pad crlf r12 True ty c
{-# INLINE hd #-}

lineW :: W -> BS -> Common -> V3 -> V3 -> Put
lineW w ty c a b p0 = hd w ty c p0 >>= mk w "AcDbLine" >>= p3 w 10 a >>= p3 w 11 b

vertexW :: W -> BS -> Common -> Vertex -> Put
vertexW w ty c v p0 =
  hd w ty c p0 >>= mk w "AcDbVertex" >>= mk w (vertexMarker (vxFlags v))
    >>= p3 w 10 (vxLocation v) >>= oD w 40 (vxStartWidth v) >>= oD w 41 (vxEndWidth v) >>= oD w 42 (vxBulge v)
    >>= pI w 70 (vxFlags v) >>= oD w 50 (vxTangent v)
    >>= oI w 71 (vxIndex1 v) >>= oI w 72 (vxIndex2 v) >>= oI w 73 (vxIndex3 v) >>= oI w 74 (vxIndex4 v)
    >>= oI w 91 (vxId v)

polylineW :: W -> BS -> Common -> Polyline -> Put
polylineW w ty c q p0 =
  hd w ty c p0 >>= mk w (polylineMarker (plFlags q)) >>= pI w 66 (plFollow q) >>= p3 w 10 (plElevation q)
    >>= pI w 70 (plFlags q) >>= oD w 40 (plStartWidth q) >>= oD w 41 (plEndWidth q)
    >>= oI w 71 (plMCount q) >>= oI w 72 (plNCount q) >>= oI w 73 (plMDensity q) >>= oI w 74 (plNDensity q)
    >>= oI w 75 (plSmoothType q) >>= oD w 39 (plThickness q) >>= o3 w 210 (plExtrusion q)

insertW :: W -> BS -> Common -> Insert -> Put
insertW w ty c i p0 =
  hd w ty c p0 >>= mk w "AcDbBlockReference" >>= oI w 66 (inFollow i) >>= pS w 2 (inName i)
    >>= p3 w 10 (inInsert i) >>= oD w 41 (inXScale i) >>= oD w 42 (inYScale i) >>= oD w 43 (inZScale i)
    >>= oD w 50 (inRotation i) >>= oI w 70 (inColumns i) >>= oI w 71 (inRows i)
    >>= oD w 44 (inColSpace i) >>= oD w 45 (inRowSpace i) >>= o3 w 210 (inExtrusion i)
