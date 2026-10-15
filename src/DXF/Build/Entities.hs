{-# LANGUAGE OverloadedStrings #-}
-- | Entities to put in a drawing, made from nothing: geometry, text,
-- polylines, inserts, hatches; and the attributes any entity takes (layer,
-- colour, linetype, lineweight, extended data).  They carry no handle: the
-- builder ("DXF.Build") gives each one when the drawing is made.
--
-- Strings are bytes in the drawing's encoding (UTF-8 from R2007, the code
-- page before): see 'DXF.Build.drawingText'.
module DXF.Build.Entities
  ( -- * Geometry
    line, point, circle, arc, face3d
  , lwpolyline, lwpolylineBulge, polyline2d, polyline3d, lwToPolyline
    -- * Text, inserts, hatches
  , text, mtext, insert, hatchSolid, hatchPattern, ansi31, mtextParts
    -- * Attributes
  , onLayer, withColor, withLinetype, withLineweight, withXData
  , rotated, scaled, alignedText, textStyle
    -- * Extents
  , entityPoints
  ) where

import qualified Data.ByteString as B
import DXF.Entity
import DXF.Entity.Geometry
import DXF.Entity.Hatch
import DXF.Entity.Poly
import DXF.Entity.Text
import DXF.Vec
import DXF.XData

mk :: B.ByteString -> Body -> Entity
mk ty b = Entity ty blankCommon b []

-- ------------------------------------------------------------------ geometry

line :: V3 -> V3 -> Entity
line a b = mk "LINE" (BGeo (GLine (Line a b Nothing Nothing)))

point :: V3 -> Entity
point p = mk "POINT" (BGeo (GPoint (Point p Nothing Nothing Nothing)))

circle :: V3 -> Double -> Entity
circle c r = mk "CIRCLE" (BGeo (GCircle (Circle c r Nothing Nothing)))

-- | Centre, radius, start and end angle in degrees, counter-clockwise.
arc :: V3 -> Double -> Double -> Double -> Entity
arc c r a b = mk "ARC" (BGeo (GArc (Arc c r a b Nothing Nothing)))

-- | A triangle repeats its third corner as the fourth.
face3d :: V3 -> V3 -> V3 -> V3 -> Entity
face3d a b c d = mk "3DFACE" (BGeo (G3dFace (Face3d a b c d Nothing)))

-- | A light-weight polyline (R2000 and later; in R12 it becomes a POLYLINE):
-- closed or not, and its vertices.
lwpolyline :: Bool -> [V2] -> Entity
lwpolyline closed ps = lwpolylineBulge closed [(p, 0) | p <- ps]

-- | The same with a bulge (tan of a quarter of the arc's angle) on each
-- vertex, for the segment that starts there.
lwpolylineBulge :: Bool -> [(V2, Double)] -> Entity
lwpolylineBulge closed vs = mk "LWPOLYLINE" (BPoly (PLwPolyline (LwPolyline (if closed then 1 else 0)
  Nothing Nothing Nothing Nothing [LwVertex x y Nothing Nothing (if b == 0 then Nothing else Just b) Nothing | (V2 x y, b) <- vs])))

-- | A heavy 2D polyline: the POLYLINE of R12.
polyline2d :: Bool -> [(V2, Double)] -> Entity
polyline2d closed vs = polyline (if closed then 1 else 0) 0 [vertex (V3 x y 0) 0 (if b == 0 then Nothing else Just b) | (V2 x y, b) <- vs]

polyline3d :: Bool -> [V3] -> Entity
polyline3d closed ps = polyline (if closed then 9 else 8) 32 [vertex p 32 Nothing | p <- ps]

polyline :: Int -> Int -> [Entity] -> Entity
polyline fl _ vs = (mk "POLYLINE" (BPoly (PPolyline (Polyline 1 v3zero fl Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing))))
  { eKids = vs ++ [mk "SEQEND" (BPoly PSeqEnd)] }

vertex :: V3 -> Int -> Maybe Double -> Entity
vertex p fl b = mk "VERTEX" (BPoly (PVertex (Vertex p Nothing Nothing b fl Nothing Nothing Nothing Nothing Nothing Nothing)))

-- | An LWPOLYLINE as the POLYLINE R12 knows (other entities unchanged).
lwToPolyline :: Entity -> Entity
lwToPolyline e = case eBody e of
  BPoly (PLwPolyline lw) ->
    let p = polyline2d (lwFlags lw `mod` 2 == 1) [(V2 (lvX v) (lvY v), maybe 0 id (lvBulge v)) | v <- lwVertices lw]
        z = maybe 0 id (lwElevation lw)
    in p { eCommon = eCommon e
         , eKids = [k { eBody = case eBody k of
                          BPoly (PVertex v) -> BPoly (PVertex v { vxLocation = (let V3 x y _ = vxLocation v in V3 x y z) })
                          b -> b } | k <- eKids p] }
  _ -> e

-- ---------------------------------------------------------------------- text

-- | Text: its content, the insertion point (the baseline's left end), the height.
text :: B.ByteString -> V3 -> Double -> Entity
text s p h = mk "TEXT" (BTxt (TText (Text (TextData p h s n n n n n n n n n n) n)))
  where n = Nothing

-- | Multi-line text with the formatting codes of AutoCAD (@\\P@ is a new line).
-- The insertion point is the top left corner.
mtext :: B.ByteString -> V3 -> Double -> Entity
mtext s p h = mk "MTEXT" (BTxt (TMText (MText p h n (Just 1) (Just 1) (mtextParts s) n n n n n n n n n n n n n n n)))
  where n = Nothing

-- | The parts of an MTEXT: chunks of at most 250 bytes, not cutting a UTF-8
-- character, the last one the tail.
mtextParts :: B.ByteString -> [B.ByteString]
mtextParts s
  | B.length s <= 250 = [s]
  | otherwise = let (a, b) = B.splitAt (cut 250) s in a : mtextParts b
  where cut k = if k > 1 && k < B.length s && B.index s k >= 0x80 && B.index s k < 0xC0 then cut (k - 1) else k

-- | The block of that name, inserted at a point.
insert :: B.ByteString -> V3 -> Entity
insert name p = mk "INSERT" (BPoly (PInsert (Insert Nothing name p n n n n n n n n n)))
  where n = Nothing

-- | A solid fill of the areas inside the closed polygons (the first is the
-- outer boundary, the others islands).
hatchSolid :: [[V2]] -> Entity
hatchSolid = hatch "SOLID" 1 Nothing Nothing Nothing

-- | A pattern fill: the name, angle, scale and the pattern's lines (as
-- 'ansi31' gives them for a scale).
hatchPattern :: B.ByteString -> Double -> Double -> [PatLine] -> [[V2]] -> Entity
hatchPattern name ang sc ls = hatch name 0 (Just ang) (Just sc) (Just ls)

-- | The lines of the ANSI31 pattern (45 degree hatching) at a scale.
ansi31 :: Double -> [PatLine]
ansi31 s = [PatLine 45 (V2 0 0) (V2 (-2.2450640303 * s) (2.2450640303 * s)) []]

hatch :: B.ByteString -> Int -> Maybe Double -> Maybe Double -> Maybe [PatLine] -> [[V2]] -> Entity
hatch name solid ang sc ls paths = mk "HATCH" (BHatch (BHatchE (Hatch (Just hd) [])))
  where
    hd = HatchData v3zero (Just (V3 0 0 1)) name solid (Just 0) (zipWith path [0 :: Int ..] paths)
           (Just (if solid == 1 then 0 else 1)) (Just 1) ang sc (fmap (const 0) ls) ls Nothing (Just []) []
    path i vs = PolyPath (if i == 0 then 3 else 18) 0 1 [(v, Nothing) | v <- vs] (Just [])

-- ----------------------------------------------------------------- attributes

onLayer :: B.ByteString -> Entity -> Entity
onLayer l = common (\c -> c { cLayer = Just l })

-- | An AutoCAD colour index (1 red ... 7 white, 256 by layer, 0 by block).
withColor :: Int -> Entity -> Entity
withColor k = common (\c -> c { cColor = Just k })

withLinetype :: B.ByteString -> Entity -> Entity
withLinetype l = common (\c -> c { cLinetype = Just l })

-- | In hundredths of a millimetre (25 is 0.25 mm; -1 by layer, -2 by block, -3 default).
withLineweight :: Int -> Entity -> Entity
withLineweight w = common (\c -> c { cLineweight = Just w })

-- | Attach what an application stored: after what is already attached.
withXData :: XData -> Entity -> Entity
withXData x e = common (\c -> c { cXData = cXData c ++ xdataTags x }) e

common :: (Common -> Common) -> Entity -> Entity
common f e = e { eCommon = f (eCommon e) }

-- | Rotate (degrees) a TEXT or an INSERT; an MTEXT gets the matching direction.
rotated :: Double -> Entity -> Entity
rotated a e = case eBody e of
  BTxt (TText t) -> e { eBody = BTxt (TText t { txData = (txData t) { tdRotation = Just a } }) }
  BTxt (TMText m) -> e { eBody = BTxt (TMText m { mtDirection = Just (V3 (cos r) (sin r) 0) }) }
  BPoly (PInsert i) -> e { eBody = BPoly (PInsert i { inRotation = Just a }) }
  _ -> e
  where r = a * pi / 180

-- | Scale an INSERT by x, y and z factors.
scaled :: Double -> Double -> Double -> Entity -> Entity
scaled x y z e = case eBody e of
  BPoly (PInsert i) -> e { eBody = BPoly (PInsert i { inXScale = Just x, inYScale = Just y, inZScale = Just z }) }
  _ -> e

-- | Horizontal (0 left, 1 centre, 2 right) and vertical (0 baseline, 1 bottom,
-- 2 middle, 3 top) alignment of a TEXT about its insertion point.
alignedText :: Int -> Int -> Entity -> Entity
alignedText h v e = case eBody e of
  BTxt (TText t) -> let d = txData t in
    e { eBody = BTxt (TText t { txData = d { tdHAlign = Just h, tdAlign = Just (tdInsert d) }, txValign = Just v }) }
  _ -> e

-- | The text style of a TEXT or an MTEXT.
textStyle :: B.ByteString -> Entity -> Entity
textStyle s e = case eBody e of
  BTxt (TText t) -> e { eBody = BTxt (TText t { txData = (txData t) { tdStyle = Just s } }) }
  BTxt (TMText m) -> e { eBody = BTxt (TMText m { mtStyle = Just s }) }
  _ -> e

-- -------------------------------------------------------------------- extents

-- | Points that bound the entity (a circle or arc by its whole circle's box;
-- an INSERT by its insertion point only: the builder adds its block's box).
entityPoints :: Entity -> [V3]
entityPoints e = case eBody e of
  BGeo g -> case g of
    GLine l -> [lineStart l, lineEnd l]
    GPoint p -> [pointLocation p]
    GCircle c -> around (circleCenter c) (circleRadius c)
    GArc a -> around (arcCenter a) (arcRadius a)
    G3dFace f -> [fP0 f, fP1 f, fP2 f, fP3 f]
    GSolid q -> [qP0 q, qP1 q, qP2 q, qP3 q]
    GTrace q -> [qP0 q, qP1 q, qP2 q, qP3 q]
    _ -> []
  BPoly (PLwPolyline lw) -> [V3 (lvX v) (lvY v) (maybe 0 id (lwElevation lw)) | v <- lwVertices lw]
  BPoly (PPolyline _) -> [vxLocation v | k <- eKids e, BPoly (PVertex v) <- [eBody k]]
  BPoly (PInsert i) -> [inInsert i]
  BTxt (TText t) -> [tdInsert (txData t)]
  BTxt (TMText m) -> [mtInsert m]
  BHatch (BHatchE (Hatch (Just h) _)) -> [V3 x y z | let V3 _ _ z = hElevation h, PolyPath _ _ _ vs _ <- hPaths h, (V2 x y, _) <- vs]
  _ -> []
  where around (V3 x y z) r = [V3 (x - r) (y - r) z, V3 (x + r) (y + r) z]
