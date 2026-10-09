{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Entities read straight from the bytes of a text file, with no tags
-- built: the common entities, in the order files write them.
--
-- A decoder here accepts only the usual shape of an entity -- the common
-- head (handle, owner, @AcDbEntity@, layer, linetype, color...), then the
-- entity's own codes in order, then the next @0@ -- and gives up
-- ('Nothing') at anything else: the tags go the general way
-- ("DXF.Codec"), which takes every shape.  What a fast decoder accepts it
-- reads exactly as the general one does; the test suite compares them on
-- every sample.
module DXF.Entity.Fast
  ( fastEntity, shareCommon
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr)
import DXF.Entity.Common
import DXF.Entity.Geometry
import DXF.Entity.Types
import DXF.Number
import DXF.Tag.Text
import DXF.Vec

-- | The entity whose type tag has the value @ty@ (a slice of @bs@), if it
-- is one of the fast ones and has the usual shape; and the offset of the
-- tag after it.  The entity before it is where layer and owner are shared from.
fastEntity :: B.ByteString -> Maybe Entity -> B.ByteString -> Maybe (Entity, Int)
fastEntity bs prev ty
  | ty == "LINE" = start >>= fastLine bs com
  | otherwise = Nothing
  where
    com = eCommon <$> prev
    -- the offset after the type tag's line
    start = let BI.BS fp _ = bs
                BI.BS fq _ = ty
                off = unsafeForeignPtrToPtr fq `minusPtr` unsafeForeignPtrToPtr fp
                e = off + B.length ty
            in if off < 0 || e > B.length bs || slice bs off e /= ty then Nothing
               else case B.elemIndex 10 (B.drop e bs) of
                      Just k -> Just (e + k + 1)
                      Nothing -> Nothing

-- | A common part and the offset of the first tag that is not part of it.
data Head = Head !Common {-# UNPACK #-} !Int

-- | Reuse the 'Just' of the previous entity's layer, linetype and owner
-- when equal: the same few strings come on every entity of a drawing.
share :: Maybe B.ByteString -> B.ByteString -> Maybe B.ByteString
share old v = case old of
  Just p | p == v -> old
  _ -> Just v
{-# INLINE share #-}

-- | The same, for a decoded entity: layer, linetype and owner take the
-- previous entity's when equal.
shareCommon :: Maybe Common -> Common -> Common
shareCommon Nothing c = c
shareCommon (Just p) c
  | any needs [cLayer, cOwner, cLinetype] = c { cLayer = sh cLayer, cOwner = sh cOwner, cLinetype = sh cLinetype }
  | otherwise = c
  where
    sh f = case f c of Nothing -> Nothing; Just v -> share (f p) v
    -- equal to the previous one's, but not the same bytes in memory
    needs f = case (f p, f c) of
      (Just a, Just b) -> a == b && addr a /= addr b
      _ -> False
    addr x = let BI.BS fx _ = x in unsafeForeignPtrToPtr fx

-- | The common part: handle (5), owner (330), @AcDbEntity@ (100), layer
-- (8), linetype (6), color (62), paperspace (67), lineweight (370).
fastHead :: B.ByteString -> Maybe Common -> Int -> Head
fastHead bs prev = go False Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  where
    pOwner = prev >>= cOwner
    pLayer = prev >>= cLayer
    pLt = prev >>= cLinetype
    go !ent h o l lt col ps lw !p = case nextTag bs p of
      T c s e nx
        | c == 5, not ent, Nothing <- h -> go ent (Just v) o l lt col ps lw nx
        | c == 330, not ent, Nothing <- o -> go ent h (share pOwner v) l lt col ps lw nx
        | c == 100, not ent, v == "AcDbEntity" -> go True h o l lt col ps lw nx
        | c == 8, Nothing <- l -> go ent h o (share pLayer v) lt col ps lw nx
        | c == 6, Nothing <- lt -> go ent h o l (share pLt v) col ps lw nx
        | c == 62, Nothing <- col, Just k <- readInt v -> go ent h o l lt (Just k) ps lw nx
        | c == 67, Nothing <- ps, Just k <- readInt v -> go ent h o l lt col (Just k) lw nx
        | c == 370, Nothing <- lw, Just k <- readInt v -> go ent h o l lt col ps (Just k) nx
        | otherwise -> Head (Common h o [] l lt col ps lw Nothing blankMore [] []) p
        where !v = slice bs s e

-- | A point: x at code c, y at c+10, and z at c+20 when it is there.
data P = P {-# UNPACK #-} !V3 {-# UNPACK #-} !Int

point3 :: B.ByteString -> Int -> Int -> Maybe P
point3 bs c p0 = case nextTag bs p0 of
  T c1 s1 e1 n1 | c1 == c, Just x <- readDouble (slice bs s1 e1) -> case nextTag bs n1 of
    T c2 s2 e2 n2 | c2 == c + 10, Just y <- readDouble (slice bs s2 e2) -> case nextTag bs n2 of
      T c3 s3 e3 n3 | c3 == c + 20 -> case readDouble (slice bs s3 e3) of
                        Just z -> Just (P (V3 x y z) n3)
                        Nothing -> Nothing
      _ -> Just (P (V3 x y 0) n2)
    _ -> Nothing
  _ -> Nothing

-- | An optional point.
optPoint :: B.ByteString -> Int -> Int -> Maybe (Maybe V3, Int)
optPoint bs c p = case nextTag bs p of
  T c1 _ _ _ | c1 == c -> (\(P v q) -> (Just v, q)) <$> point3 bs c p
  _ -> Just (Nothing, p)

-- | An optional double.
optD :: B.ByteString -> Int -> Int -> Maybe (Maybe Double, Int)
optD bs c p = case nextTag bs p of
  T c1 s e nx | c1 == c -> (\d -> (Just d, nx)) <$> readDouble (slice bs s e)
  _ -> Just (Nothing, p)

-- | Skip a subclass marker, if it is the next tag.
marker :: B.ByteString -> B.ByteString -> Int -> Int
marker bs name p = case nextTag bs p of
  T 100 s e nx | slice bs s e == name -> nx
  _ -> p

-- | The entity is over: the next tag is a @0@.
isEnd :: B.ByteString -> Int -> Bool
isEnd bs p = case nextTag bs p of
  T 0 _ _ _ -> True
  _ -> False

fastLine :: B.ByteString -> Maybe Common -> Int -> Maybe (Entity, Int)
fastLine bs prev p0 = case fastHead bs prev p0 of
  Head com p1 -> do
    (th1, p2) <- optD bs 39 (marker bs "AcDbLine" p1)   -- AutoCAD writes the thickness first
    P a p3 <- point3 bs 10 p2
    P b p4 <- point3 bs 11 p3
    (th2, p5) <- maybe (optD bs 39 p4) (const (Just (Nothing, p4))) th1
    (ext, p6) <- optPoint bs 210 p5
    if isEnd bs p6
      then Just (Entity "LINE" com (BGeo (GLine (Line a b (maybe th2 Just th1) ext))) [], p6)
      else Nothing
