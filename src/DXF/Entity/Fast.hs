{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Entities read straight from the bytes of a text file, with no tags
-- built: the usual ones, in the order files write them.
--
-- A decoder here accepts only the usual shape of an entity -- the common
-- head (handle, owner, @AcDbEntity@, layer, linetype, color...), then the
-- entity's own codes in order, then the next @0@ -- and gives up
-- ('Nothing') at anything else: the tags go the general way
-- ("DXF.Codec"), which takes every shape.  What a fast decoder accepts it
-- reads exactly as the general one does; the test suite compares them on
-- every sample and fixture.
module DXF.Entity.Fast
  ( fastEntity
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr)
import DXF.Entity.Common
import DXF.Entity.FastKit
import DXF.Entity.Geometry
import DXF.Entity.Poly
import DXF.Entity.Text
import DXF.Entity.Types
import DXF.Intern
import DXF.Tag.Text

-- | The entity whose type tag has the value @ty@ (a slice of @bs@), if it
-- is one of the fast ones and has the usual shape; and the offset of the
-- tag after it.  Layer, linetype, owner, styles and block names are interned.
fastEntity :: Interner -> B.ByteString -> B.ByteString -> Maybe (Entity, Int)
fastEntity tbl bs ty = case start of
  Nothing -> Nothing
  Just p -> case ty of
    "VERTEX" -> run p vertexR
    "LINE" -> run p lineR
    "TEXT" -> run p textR
    "POLYLINE" -> run p polylineR
    "SEQEND" -> run p seqendR
    "INSERT" -> run p insertR
    "CIRCLE" -> run p circleR
    "ARC" -> run p arcR
    "POINT" -> run p pointR
    "LWPOLYLINE" -> run p lwpolyR
    _ -> Nothing
  where
    run p r = runR tbl bs p r
    -- the offset after the type tag's line
    start = let BI.BS fp _ = bs
                BI.BS fq _ = ty
                off = unsafeForeignPtrToPtr fq `minusPtr` unsafeForeignPtrToPtr fp
                e = off + B.length ty
            in if off < 0 || e > B.length bs || slice bs off e /= ty then Nothing
               else case B.elemIndex 10 (B.drop e bs) of
                      Just k -> Just (e + k + 1)
                      Nothing -> Nothing

ent :: B.ByteString -> Common -> Body -> Entity
ent ty c b = Entity ty c b []
{-# INLINE ent #-}

-- | AutoCAD writes the thickness (39) before the geometry, the codecs
-- after it: one place or the other, not both.
thick :: Maybe Double -> R (Maybe Double)
thick (Just d) = pure (Just d)
thick Nothing = mdbl 39

-- ------------------------------------------------------------------ geometry

lineR :: R Entity
lineR = do
  c <- commonR
  marks ["AcDbLine"]
  t1 <- mdbl 39
  a <- pt 10
  b <- pt 11
  t2 <- thick t1
  x <- mpt 210
  end
  pure (ent "LINE" c (BGeo (GLine (Line a b t2 x))))

pointR :: R Entity
pointR = do
  c <- commonR
  marks ["AcDbPoint"]
  t1 <- mdbl 39
  p <- pt 10
  t2 <- thick t1
  x <- mpt 210
  a <- mdbl 50
  end
  pure (ent "POINT" c (BGeo (GPoint (Point p t2 x a))))

circleR :: R Entity
circleR = do
  c <- commonR
  marks ["AcDbCircle"]
  t1 <- mdbl 39
  p <- pt 10
  r <- dbl 40
  t2 <- thick t1
  x <- mpt 210
  end
  pure (ent "CIRCLE" c (BGeo (GCircle (Circle p r t2 x))))

arcR :: R Entity
arcR = do
  c <- commonR
  marks ["AcDbCircle"]
  t1 <- mdbl 39
  p <- pt 10
  r <- dbl 40
  t2 <- thick t1
  x <- mpt 210
  marks ["AcDbArc"]
  a <- dbl 50
  b <- dbl 51
  end
  pure (ent "ARC" c (BGeo (GArc (Arc p r a b t2 x))))

-- ------------------------------------------------------------------ polylines

vertexMarks :: [B.ByteString]
vertexMarks = [ "AcDbVertex", "AcDb2dVertex", "AcDb3dPolylineVertex", "AcDbPolygonMeshVertex"
              , "AcDbPolyFaceMeshVertex", "AcDbFaceRecord" ]

vertexR :: R Entity
vertexR = do
  c <- commonR
  marks vertexMarks
  p <- pt 10
  sw <- mdbl 40
  ew <- mdbl 41
  bu <- mdbl 42
  fl <- intD 0 70
  tg <- mdbl 50
  i1 <- mint 71
  i2 <- mint 72
  i3 <- mint 73
  i4 <- mint 74
  vid <- mint 91
  end
  pure (ent "VERTEX" c (BPoly (PVertex (Vertex p sw ew bu fl tg i1 i2 i3 i4 vid))))

polylineR :: R Entity
polylineR = do
  c <- commonR
  marks ["AcDb2dPolyline", "AcDb3dPolyline", "AcDbPolygonMesh", "AcDbPolyFaceMesh"]
  t1 <- mdbl 39
  fo <- intD 1 66
  el <- ptD 10
  fl <- intD 0 70
  sw <- mdbl 40
  ew <- mdbl 41
  m <- mint 71
  n <- mint 72
  md <- mint 73
  nd <- mint 74
  sm <- mint 75
  t2 <- thick t1
  x <- mpt 210
  end
  pure (ent "POLYLINE" c (BPoly (PPolyline (Polyline fo el fl sw ew m n md nd sm t2 x))))

seqendR :: R Entity
seqendR = do
  c <- commonR
  end
  pure (ent "SEQEND" c (BPoly PSeqEnd))

insertR :: R Entity
insertR = do
  c <- commonR
  marks ["AcDbBlockReference"]
  fo <- mint 66
  nm <- strI 2
  p <- pt 10
  xs <- mdbl 41
  ys <- mdbl 42
  zs <- mdbl 43
  ro <- mdbl 50
  co <- mint 70
  rw <- mint 71
  cs <- mdbl 44
  rs <- mdbl 45
  x <- mpt 210
  end
  pure (ent "INSERT" c (BPoly (PInsert (Insert fo nm p xs ys zs ro co rw cs rs x))))

lwpolyR :: R Entity
lwpolyR = do
  c <- commonR
  marks ["AcDbPolyline"]
  _ <- mint 90
  fl <- intD 0 70
  cw <- mdbl 43
  el <- mdbl 38
  th <- mdbl 39
  vs <- vertices []
  x <- mpt 210
  end
  pure (ent "LWPOLYLINE" c (BPoly (PLwPolyline (LwPolyline fl cw el th x vs))))
  where
    vertices acc = do
      k <- peek
      if k /= 10 then pure (reverse acc) else do
        px <- dbl 10
        py <- dbl 20
        i1 <- mint 91
        sw <- mdbl 40
        ew <- mdbl 41
        bu <- mdbl 42
        i2 <- maybe (mint 91) (const (pure Nothing)) i1
        vertices (LwVertex px py sw ew bu (i1 <|> i2) : acc)

-- ---------------------------------------------------------------------- text

textR :: R Entity
textR = do
  c <- commonR
  marks ["AcDbText"]
  th <- mdbl 39
  p <- pt 10
  h <- dbl 40
  tx <- str 1
  ro <- mdbl 50
  w <- mdbl 41
  ob <- mdbl 51
  st <- mstrI 7
  el <- mdbl 38
  gf <- mint 71
  ha <- mint 72
  al <- mpt 11
  x <- mpt 210
  marks ["AcDbText"]
  va <- mint 73
  end
  pure (ent "TEXT" c (BTxt (TText (Text (TextData p h tx ro w ob st gf ha al th el x) va))))
