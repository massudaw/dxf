{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Describing an entity once, for reading and writing.
--
-- A 'Codec' for a record @r@ is a list of fields ('F'): each says which
-- group codes it reads, how to write itself, and what it shows in a dump
-- (named as ezdxf names the attribute, so the dump can be compared with
-- ezdxf's).  Reading folds the tags into the record; a tag no field takes
-- -- an unknown code, a value that is not a number -- is handed back, so
-- nothing is lost.  Optional fields are 'Maybe' and written only when set.
module DXF.Codec
  ( F (..), L, lens
  , dbl, mdbl, int, mint, str, mstr, flag
  , pt3, mpt3, pt2, mpt2, dbls, ints, strs, pts3, pts2
  , mark, markS, markR, markRS, custom, under
  , Codec, codec, decodeWith, encodeWith, dumpWith
  , BodyCodec (..), entityCodec
  ) where

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM
import DXF.Json
import DXF.Number
import DXF.Tag
import DXF.Tag.Out
import DXF.Vec

type Name = B.ByteString

-- | One field of a record @b@.
data F b = F
  { fName   :: Name                         -- ^ ezdxf's attribute name; empty: not dumped
  , fCodes  :: [Int]                        -- ^ group codes this field reads
  , fRead   :: Int -> B.ByteString -> b -> Maybe b  -- ^ 'Nothing': not understood, keep the tag
  , fWrite  :: b -> Out
  , fDump   :: b -> Maybe J
  , fFin    :: b -> b                       -- ^ after the last tag (lists are read reversed)
  , fMarker :: Maybe (B.ByteString, b -> b) -- ^ a subclass marker this field writes; reading it applies the function
  }

-- | A getter and a setter.
data L b a = L (b -> a) (a -> b -> b)

lens :: (b -> a) -> (b -> a -> b) -> L b a
lens g s = L g (flip s)

get :: L b a -> b -> a
get (L g _) = g

set :: L b a -> a -> b -> b
set (L _ s) = s

simple :: Name -> [Int] -> (Int -> B.ByteString -> b -> Maybe b) -> (b -> Out) -> (b -> Maybe J) -> F b
simple n cs r w d = F n cs r w d id Nothing

-- | A required double.
dbl :: Name -> Int -> L b Double -> F b
dbl n c l = simple n [c] (\_ v b -> (\d -> set l d b) <$> readDouble v) (tD c . get l) (Just . JD . get l)

-- | An optional double.
mdbl :: Name -> Int -> L b (Maybe Double) -> F b
mdbl n c l = simple n [c] (\_ v b -> unset l b >> (\d -> set l (Just d) b) <$> readDouble v)
  (tMaybeD c . get l) (fmap JD . get l)

int :: Name -> Int -> L b Int -> F b
int n c l = simple n [c] (\_ v b -> (\d -> set l d b) <$> readInt v) (tI c . get l) (Just . JI . get l)

mint :: Name -> Int -> L b (Maybe Int) -> F b
mint n c l = simple n [c] (\_ v b -> unset l b >> (\d -> set l (Just d) b) <$> readInt v) (tMaybeI c . get l) (fmap JI . get l)

-- | A string, as the file's bytes.
str :: Name -> Int -> L b B.ByteString -> F b
str n c l = simple n [c] (\_ v b -> Just (set l v b)) (tS c . get l) (Just . JS . get l)

mstr :: Name -> Int -> L b (Maybe B.ByteString) -> F b
mstr n c l = simple n [c] (\_ v b -> unset l b >> Just (set l (Just v) b)) (tMaybeS c . get l) (fmap JS . get l)

-- | An optional 0/1 flag.
flag :: Name -> Int -> L b (Maybe Bool) -> F b
flag n c l = simple n [c] (\_ v b -> unset l b >> (\d -> set l (Just (d /= 0)) b) <$> readInt v)
  (maybe mempty (tB c) . get l) (fmap (JI . fromEnum) . get l)

-- | A 'Maybe' field that is still empty: a second tag for it is left alone.
unset :: L b (Maybe a) -> b -> Maybe ()
unset l b = maybe (Just ()) (const Nothing) (get l b)

setX, setY, setZ :: Double -> V3 -> V3
setX d (V3 _ y z) = V3 d y z
setY d (V3 x _ z) = V3 x d z
setZ d (V3 x y _) = V3 x y d

-- | A required 3D point: x, y, z at codes c, c+10, c+20.  A missing z is 0.
pt3 :: Name -> Int -> L b V3 -> F b
pt3 n c l = simple n [c, c + 10, c + 20] rd (tV3 c . get l) (Just . jPoint3 . get l)
  where rd k v b = (\d -> set l (comp (k - c) d (get l b)) b) <$> readDouble v
        comp 0 = setX
        comp 10 = setY
        comp _ = setZ

-- | An optional 3D point.
mpt3 :: Name -> Int -> L b (Maybe V3) -> F b
mpt3 n c l = simple n [c, c + 10, c + 20] rd (maybe mempty (tV3 c) . get l) (fmap jPoint3 . get l)
  where rd k v b = (\d -> set l (Just (comp (k - c) d (maybe v3zero id (get l b)))) b) <$> readDouble v
        comp 0 = setX
        comp 10 = setY
        comp _ = setZ

pt2 :: Name -> Int -> L b V2 -> F b
pt2 n c l = simple n [c, c + 10] rd (tV2 c . get l) (Just . jPoint2 . get l)
  where rd k v b = (\d -> set l (comp (k - c) d (get l b)) b) <$> readDouble v
        comp 0 d (V2 _ y) = V2 d y
        comp _ d (V2 x _) = V2 x d

mpt2 :: Name -> Int -> L b (Maybe V2) -> F b
mpt2 n c l = simple n [c, c + 10] rd (maybe mempty (tV2 c) . get l) (fmap jPoint2 . get l)
  where rd k v b = (\d -> set l (Just (comp (k - c) d (maybe v2zero id (get l b)))) b) <$> readDouble v
        comp 0 d (V2 _ y) = V2 d y
        comp _ d (V2 x _) = V2 x d

-- | A repeated group code, in order.
repeated :: Name -> Int -> (B.ByteString -> Maybe a) -> (a -> Out) -> (a -> J) -> L b [a] -> F b
repeated n c parse w j l = F n [c] (\_ v b -> (\x -> set l (x : get l b) b) <$> parse v)
  (foldMap w . get l) (Just . JA . map j . get l) (\b -> set l (reverse (get l b)) b) Nothing

dbls :: Name -> Int -> L b [Double] -> F b
dbls n c = repeated n c readDouble (tD c) JD

ints :: Name -> Int -> L b [Int] -> F b
ints n c = repeated n c readInt (tI c) JI

strs :: Name -> Int -> L b [B.ByteString] -> F b
strs n c = repeated n c Just (tS c) JS

-- | A list of points, each started by its x (code c).
pts3 :: Name -> Int -> L b [V3] -> F b
pts3 n c l = F n [c, c + 10, c + 20] rd (foldMap (tV3 c) . get l) (Just . JA . map jPoint3 . get l)
  (\b -> set l (reverse (get l b)) b) Nothing
  where
    rd k v b = readDouble v >>= \d -> case (k - c, get l b) of
      (0, ps) -> Just (set l (V3 d 0 0 : ps) b)
      (10, V3 x _ z : ps) -> Just (set l (V3 x d z : ps) b)
      (_, V3 x y _ : ps) -> Just (set l (V3 x y d : ps) b)
      _ -> Nothing

pts2 :: Name -> Int -> L b [V2] -> F b
pts2 n c l = F n [c, c + 10] rd (foldMap (tV2 c) . get l) (Just . JA . map jPoint2 . get l)
  (\b -> set l (reverse (get l b)) b) Nothing
  where
    rd k v b = readDouble v >>= \d -> case (k - c, get l b) of
      (0, ps) -> Just (set l (V2 d 0 : ps) b)
      (_, V2 x _ : ps) -> Just (set l (V2 x d : ps) b)
      _ -> Nothing

-- | A subclass marker: written (after R12), and dropped when read.
mark :: B.ByteString -> F b
mark m = markS m id

-- | A marker that also changes the record when read (the codes after it mean something else).
markS :: B.ByteString -> (b -> b) -> F b
markS m f = F "" [] (\_ _ _ -> Nothing) (const (tMarker m)) (const Nothing) id (Just (m, f))

-- | A marker that changes the record when read and is never written.
markRS :: B.ByteString -> (b -> b) -> F b
markRS m f = (markR m) { fMarker = Just (m, f) }

-- | A marker that is dropped when read and never written.
markR :: B.ByteString -> F b
markR m = F "" [] (\_ _ _ -> Nothing) (const mempty) (const Nothing) id (Just (m, id))

-- | A field of a part of the record, as a field of the record.
under :: (b -> a) -> (a -> b -> b) -> F a -> F b
under g s f = F (fName f) (fCodes f) (\c v b -> (\a -> s a b) <$> fRead f c v (g b)) (fWrite f . g)
  (fDump f . g) (\b -> s (fFin f (g b)) b) (fmap (\(m, h) -> (m, \b -> s (h (g b)) b)) (fMarker f))

-- | Anything else: codes read, a reader, a writer, a dump entry.
custom :: Name -> [Int] -> (Int -> B.ByteString -> b -> Maybe b) -> (b -> Out) -> (b -> Maybe J) -> (b -> b) -> F b
custom n cs r w d fin = F n cs r w d fin Nothing

-- | The fields of a record, with an empty record to read into.
data Codec b = Codec
  { cdMarkers :: [(B.ByteString, b -> b)]
  , cdTable   :: IM.IntMap (F b)
  , cdFields  :: [F b]
  , cdBlank   :: b
  , cdFin     :: b -> b
  }

codec :: b -> [F b] -> Codec b
codec blank fs = Codec [m | f <- fs, Just m <- [fMarker f]]
  (IM.fromList [(c, f) | f <- fs, c <- fCodes f]) fs blank (foldr ((.) . fFin) id fs)

-- | Fold the tags into the record; the tags no field took, in order.
decodeWith :: Codec b -> [Tag] -> (b, [Tag])
decodeWith (Codec mk tbl _ blank fin) = go blank []
  where
    go !r acc [] = (fin r, reverse acc)
    go !r acc ts@(Tag 101 _ : _) = (fin r, reverse acc ++ ts)   -- an embedded object: not ours
    go !r acc (t@(Tag c v) : ts)
      | c == 100, Just h <- lookup v mk = go (h r) acc ts
      | Just f <- IM.lookup c tbl, Just r' <- fRead f c v r = go r' acc ts
      | otherwise = go r (t : acc) ts

encodeWith :: Codec b -> b -> Out
encodeWith cd r = foldMap (\f -> fWrite f r) (cdFields cd)

dumpWith :: Codec b -> b -> [(B.ByteString, J)]
dumpWith cd r = [(fName f, j) | f <- cdFields cd, not (B.null (fName f)), Just j <- [fDump f r]]

-- | What the registry needs of an entity type, over the group's own sum
-- type @g@ of bodies.
data BodyCodec g = BodyCodec
  { bcType    :: B.ByteString                  -- ^ the DXF name: @LINE@
  , bcGraphic :: Bool                          -- ^ a graphical entity (has layer, color...) or an object / table entry
  , bcDecode  :: [Tag] -> (g, [Tag])
  , bcEncode  :: g -> Out
  , bcDump    :: g -> [(B.ByteString, J)]
  }

-- | A field codec as an entity type of the group @g@, by the constructor
-- that wraps its record.
entityCodec :: B.ByteString -> Bool -> Codec r -> (r -> g) -> (g -> Maybe r) -> BodyCodec g
entityCodec t gr cd wrap unwrap = BodyCodec t gr
  (\ts -> let (r, left) = decodeWith cd ts in (wrap r, left))
  (maybe mempty (encodeWith cd) . unwrap)
  (maybe [] (dumpWith cd) . unwrap)
