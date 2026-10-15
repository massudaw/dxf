{-# LANGUAGE BangPatterns, DataKinds, OverloadedStrings #-}
-- | The usual entities written as one primitive each, the whole entity one linbuf 'F.Bounded' made of
-- its tags (the optional ones 'F.choose'n), run once at the builder's address: LINE, VERTEX, POLYLINE,
-- SEQEND, INSERT.  Anything unusual (application data, extended data, unplaced tags, long strings,
-- numbers outside plain decimal, binary output) is left to the general writer, which says the same; the
-- test suite compares the two on every sample and fixture in every layout.
--
-- The children of an entity (a POLYLINE's vertices) are not written here.
--
-- The runners ('lineOne' ...) are NOINLINE and take the entity itself; the makers are INLINE, down to
-- the point ('p3B'): a maker left a function returning a 'F.Bounded' makes its choices thunks.
module DXF.Entity.FastOut
  ( fastOut
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder.Prim (primBounded)
import Data.Maybe (fromMaybe, isJust)
import LinBuf.Builder (asBoundedPrim)
import LinBuf.Fixed (Sized (..))
import qualified LinBuf.Fixed as F
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
fastOut f en@(Entity ty c b _)
  | fmtBinary f || not (plainHead ty c) || not (null (cXData c)) || not (null (cExtra c)) = Nothing
  | otherwise = case b of
      BGeo (GLine (Line a e Nothing Nothing)) | ok3 a, ok3 e -> Just (lineOne f en)
      BPoly (PVertex v) | okVertex v -> Just (vertexOne f en)
      BPoly (PPolyline p) | okPolyline p -> Just (polylineOne f en)
      BPoly PSeqEnd -> Just (headOne f True ty c)
      BPoly (PInsert i) | okInsert i -> Just (insertOne f en)
      _ -> Nothing

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

-- * The runners: one primitive for the entity.

lineOne, vertexOne, polylineOne, insertOne :: Fmt -> Entity -> BB.Builder
lineOne f = let !w = lay f in primBounded (asBoundedPrim (\en -> case en of
  Entity ty c (BGeo (GLine (Line a e _ _))) _ -> lineB w ty c a e
  _ -> F.widen none))
{-# NOINLINE lineOne #-}

vertexOne f = let !w = lay f in primBounded (asBoundedPrim (\en -> case en of
  Entity ty c (BPoly (PVertex v)) _ -> vertexB w ty c v
  _ -> F.widen none))
{-# NOINLINE vertexOne #-}

polylineOne f = let !w = lay f in primBounded (asBoundedPrim (\en -> case en of
  Entity ty c (BPoly (PPolyline q)) _ -> polylineB w ty c q
  _ -> F.widen none))
{-# NOINLINE polylineOne #-}

insertOne f = let !w = lay f in primBounded (asBoundedPrim (\en -> case en of
  Entity ty c (BPoly (PInsert i)) _ -> insertB w ty c i
  _ -> F.widen none))
{-# NOINLINE insertOne #-}

-- * The makers.

type BS = B.ByteString

-- | The layout: padded codes, CR LF, R12.
data W = W !Bool !Bool !Bool

lay :: Fmt -> W
lay f = W (fmtPad f) (fmtCRLF f) (fmtR12 f)
{-# INLINE lay #-}

dB :: W -> Int -> Double -> F.Bounded 68
dB (W pad crlf _) = doubleTagB pad crlf
{-# INLINE dB #-}

iB :: W -> Int -> Int -> F.Bounded 46
iB (W pad crlf _) k i = let !v = i in intTagB pad crlf k v
{-# INLINE iB #-}

sB :: W -> Int -> BS -> F.Bounded 90
sB (W pad crlf _) k s = let !v = s in bytesTagB pad crlf k v
{-# INLINE sB #-}

oDB :: W -> Int -> Maybe Double -> F.Bounded 68
oDB (W pad crlf _) = optDoubleB pad crlf
{-# INLINE oDB #-}

oIB :: W -> Int -> Maybe Int -> F.Bounded 46
oIB (W pad crlf _) = optIntB pad crlf
{-# INLINE oIB #-}

-- | A point at codes k, k+10, k+20.
p3B :: W -> Int -> V3 -> F.Bounded 204
p3B w k (V3 x y z) = dB w k x <+> dB w (k + 10) y <+> dB w (k + 20) z
{-# INLINE p3B #-}

o3B :: W -> Int -> Maybe V3 -> F.Bounded 204
o3B w k m = let !j = isJust m; !v = fromMaybe (V3 0 0 0) m in F.choose j (p3B w k v) (F.widen none)
{-# INLINE o3B #-}

-- | A subclass marker, but not in R12.
mkB :: W -> BS -> F.Bounded 90
mkB (W pad crlf r12) m = let !v = m in F.choose r12 (F.widen none) (bytesTagB pad crlf 100 v)
{-# INLINE mkB #-}

hdB :: W -> BS -> Common -> F.Bounded 746
hdB (W pad crlf r12) = headB pad crlf r12 True
{-# INLINE hdB #-}

lineB :: W -> BS -> Common -> V3 -> V3 -> F.Bounded 1244
lineB w ty c a e = hdB w ty c <+> mkB w "AcDbLine" <+> p3B w 10 a <+> p3B w 11 e
{-# INLINE lineB #-}

vertexB :: W -> BS -> Common -> Vertex -> F.Bounded 1678
vertexB w ty c v =
  hdB w ty c <+> mkB w "AcDbVertex" <+> mkB w (vertexMarker (vxFlags v))
    <+> p3B w 10 (vxLocation v) <+> oDB w 40 (vxStartWidth v) <+> oDB w 41 (vxEndWidth v) <+> oDB w 42 (vxBulge v)
    <+> iB w 70 (vxFlags v) <+> oDB w 50 (vxTangent v)
    <+> oIB w 71 (vxIndex1 v) <+> oIB w 72 (vxIndex2 v) <+> oIB w 73 (vxIndex3 v) <+> oIB w 74 (vxIndex4 v)
    <+> oIB w 91 (vxId v)
{-# INLINE vertexB #-}

polylineB :: W -> BS -> Common -> Polyline -> F.Bounded 1770
polylineB w ty c q =
  hdB w ty c <+> mkB w (polylineMarker (plFlags q)) <+> iB w 66 (plFollow q) <+> p3B w 10 (plElevation q)
    <+> iB w 70 (plFlags q) <+> oDB w 40 (plStartWidth q) <+> oDB w 41 (plEndWidth q)
    <+> oIB w 71 (plMCount q) <+> oIB w 72 (plNCount q) <+> oIB w 73 (plMDensity q) <+> oIB w 74 (plNDensity q)
    <+> oIB w 75 (plSmoothType q) <+> oDB w 39 (plThickness q) <+> o3B w 210 (plExtrusion q)
{-# INLINE polylineB #-}

insertB :: W -> BS -> Common -> Insert -> F.Bounded 1880
insertB w ty c i =
  hdB w ty c <+> mkB w "AcDbBlockReference" <+> oIB w 66 (inFollow i) <+> sB w 2 (inName i)
    <+> p3B w 10 (inInsert i) <+> oDB w 41 (inXScale i) <+> oDB w 42 (inYScale i) <+> oDB w 43 (inZScale i)
    <+> oDB w 50 (inRotation i) <+> oIB w 70 (inColumns i) <+> oIB w 71 (inRows i)
    <+> oDB w 44 (inColSpace i) <+> oDB w 45 (inRowSpace i) <+> o3B w 210 (inExtrusion i)
{-# INLINE insertB #-}
