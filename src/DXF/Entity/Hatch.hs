{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | HATCH: its header values, boundary paths (polyline paths and edge paths
-- of lines, arcs, ellipses and splines), pattern lines, seed points and
-- gradient.  The codes of the boundary section mean different things in
-- different places, so the tags are read by a small parser; a hatch that
-- does not fit it is kept as the tags it was ('hdRaw').
module DXF.Entity.Hatch
  ( HatchBody (..), hatchCodecs
  , Hatch (..), HatchData (..), Path (..), Edge (..), PatLine (..)
  , parseHatch, hatchTags
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.Bits (testBit)
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Json
import DXF.Number
import DXF.Tag
import DXF.Tag.Out
import DXF.Vec

newtype HatchBody = BHatchE Hatch
  deriving (Eq, Show, Generic, NFData)

-- | The HATCH subclass: parsed, or the tags when they do not parse.
data Hatch = Hatch
  { hdData :: Maybe HatchData
  , hdRaw  :: [Tag]
  } deriving (Eq, Show, Generic, NFData)

data HatchData = HatchData
  { hElevation :: !V3                -- ^ 10
  , hExtrusion :: !(Maybe V3)        -- ^ 210
  , hPatName   :: !B.ByteString      -- ^ 2
  , hSolid     :: !Int               -- ^ 70
  , hAssoc     :: !(Maybe Int)       -- ^ 71
  , hPaths     :: [Path]             -- ^ 91 counts them
  , hStyle     :: !(Maybe Int)       -- ^ 75
  , hPatType   :: !(Maybe Int)       -- ^ 76
  , hAngle     :: !(Maybe Double)    -- ^ 52
  , hScale     :: !(Maybe Double)    -- ^ 41
  , hDouble    :: !(Maybe Int)       -- ^ 77
  , hLines     :: !(Maybe [PatLine]) -- ^ 78 counts them
  , hPixel     :: !(Maybe Double)    -- ^ 47
  , hSeeds     :: !(Maybe [V2])      -- ^ 98 counts them
  , hGradient  :: [Tag]              -- ^ 450 and following
  } deriving (Eq, Show, Generic, NFData)

data Path
  = PolyPath
      { ppFlags :: !Int, ppBulge :: !Int, ppClosed :: !Int           -- ^ 92, 72, 73
      , ppVerts :: [(V2, Maybe Double)]                              -- ^ 10, with 42 when 72 says so
      , ppSources :: !(Maybe [B.ByteString]) }                       -- ^ 97 counts the 330s
  | EdgePath
      { epFlags :: !Int, epEdges :: [Edge], epSources :: !(Maybe [B.ByteString]) }
  deriving (Eq, Show, Generic, NFData)

-- | Angles as in the file (degrees; counter-clockwise flag 73).
data Edge
  = ELine !V2 !V2
  | EArc !V2 !Double !Double !Double !Int                    -- ^ center, radius, start, end, ccw
  | EEllipse !V2 !V2 !Double !Double !Double !Int            -- ^ center, major axis end, ratio, start, end, ccw
  | ESpline
      { esDegree :: !Int, esRational :: !Int, esPeriodic :: !Int
      , esKnots :: [Double], esCtrl :: [(V2, Maybe Double)]
      , esFit :: !(Maybe [V2]), esStart :: !(Maybe V2), esEnd :: !(Maybe V2) }
  deriving (Eq, Show, Generic, NFData)

data PatLine = PatLine !Double !V2 !V2 [Double]              -- ^ angle, base, offset, dashes
  deriving (Eq, Show, Generic, NFData)

-- A parser over the tags: the value and what is left.
newtype P a = P ([Tag] -> Maybe (a, [Tag]))

runP :: P a -> [Tag] -> Maybe (a, [Tag])
runP (P f) = f

instance Functor P where fmap f (P p) = P $ \ts -> fmap (\(a, r) -> (f a, r)) (p ts)
instance Applicative P where
  pure a = P $ \ts -> Just (a, ts)
  P f <*> P a = P $ \ts -> case f ts of
    Nothing -> Nothing
    Just (g, r) -> fmap (\(x, r') -> (g x, r')) (a r)
instance Monad P where
  P a >>= f = P $ \ts -> a ts >>= \(x, r) -> runP (f x) r

tag :: Int -> (B.ByteString -> Maybe a) -> P a
tag c f = P $ \ts -> case ts of
  Tag c' v : r | c' == c -> fmap (\x -> (x, r)) (f v)
  _ -> Nothing

opt :: P a -> P (Maybe a)
opt (P p) = P $ \ts -> case p ts of
  Just (x, r) -> Just (Just x, r)
  Nothing -> Just (Nothing, ts)

pI :: Int -> P Int
pI c = tag c readInt
pD :: Int -> P Double
pD c = tag c readDouble
pV2 :: Int -> P V2
pV2 c = V2 <$> pD c <*> pD (c + 10)
pV3 :: Int -> P V3
pV3 c = V3 <$> pD c <*> pD (c + 10) <*> pD (c + 20)

counted :: Int -> P a -> P [a]
counted c p = pI c >>= \n -> replicateM n p

sources :: P (Maybe [B.ByteString])
sources = opt (counted 97 (tag 330 Just))

path :: P Path
path = pI 92 >>= \fl ->
  if testBit fl 1
    then do
      b <- pI 72
      c <- pI 73
      vs <- counted 93 ((,) <$> pV2 10 <*> (if b /= 0 then Just <$> pD 42 else pure Nothing))
      PolyPath fl b c vs <$> sources
    else do
      es <- counted 93 edge
      EdgePath fl es <$> sources

edge :: P Edge
edge = pI 72 >>= \t -> case t of
  1 -> ELine <$> pV2 10 <*> pV2 11
  2 -> EArc <$> pV2 10 <*> pD 40 <*> pD 50 <*> pD 51 <*> pI 73
  3 -> EEllipse <$> pV2 10 <*> pV2 11 <*> pD 40 <*> pD 50 <*> pD 51 <*> pI 73
  4 -> do
    d <- pI 94
    r <- pI 73
    p <- pI 74
    nk <- pI 95
    nc <- pI 96
    ks <- replicateM nk (pD 40)
    cs <- replicateM nc ((,) <$> pV2 10 <*> (if r /= 0 then Just <$> pD 42 else pure Nothing))
    fit <- opt (counted 97 (pV2 11))
    st <- opt (pV2 12)
    en <- opt (pV2 13)
    pure (ESpline d r p ks cs fit st en)
  _ -> P (const Nothing)

patLine :: P PatLine
patLine = do
  a <- pD 53
  b <- V2 <$> pD 43 <*> pD 44
  o <- V2 <$> pD 45 <*> pD 46
  PatLine a b o <$> counted 79 (pD 49)

gradientCodes :: [Int]
gradientCodes = [63, 421, 450, 451, 452, 453, 460, 461, 462, 463, 470]

-- | The hatch from its subclass tags (after @AcDbHatch@), if they fit.
parseHatch :: [Tag] -> Maybe HatchData
parseHatch ts = case runP hatch ts of
  Just (h, []) | all ((\(Tag c _) -> c `elem` gradientCodes)) (hGradient h) -> Just h
  _ -> Nothing
  where
    hatch = do
      el <- pV3 10
      ex <- opt (pV3 210)
      nm <- tag 2 Just
      so <- pI 70
      as <- opt (pI 71)
      ps <- counted 91 path
      st <- opt (pI 75)
      ty <- opt (pI 76)
      an <- opt (pD 52)
      sc <- opt (pD 41)
      db <- opt (pI 77)
      ls <- opt (counted 78 patLine)
      px <- opt (pD 47)
      sd <- opt (counted 98 (pV2 10))
      rest <- P (\r -> Just (r, []))
      pure (HatchData el ex nm so as ps st ty an sc db ls px sd rest)

srcOut :: Maybe [B.ByteString] -> Out
srcOut = maybe mempty (\hs -> tI 97 (length hs) <> foldMap (tS 330) hs)

vtx :: (V2, Maybe Double) -> Out
vtx (p, b) = tV2 10 p <> tMaybeD 42 b

pathOut :: Path -> Out
pathOut (PolyPath f b c vs ss) = tI 92 f <> tI 72 b <> tI 73 c <> tI 93 (length vs) <> foldMap vtx vs <> srcOut ss
pathOut (EdgePath f es ss) = tI 92 f <> tI 93 (length es) <> foldMap edgeOut es <> srcOut ss

edgeOut :: Edge -> Out
edgeOut (ELine a b) = tI 72 1 <> tV2 10 a <> tV2 11 b
edgeOut (EArc c r s e w) = tI 72 2 <> tV2 10 c <> tD 40 r <> tD 50 s <> tD 51 e <> tI 73 w
edgeOut (EEllipse c m r s e w) = tI 72 3 <> tV2 10 c <> tV2 11 m <> tD 40 r <> tD 50 s <> tD 51 e <> tI 73 w
edgeOut (ESpline d r p ks cs fit st en) =
  tI 72 4 <> tI 94 d <> tI 73 r <> tI 74 p <> tI 95 (length ks) <> tI 96 (length cs)
  <> foldMap (tD 40) ks <> foldMap vtx cs
  <> maybe mempty (\f -> tI 97 (length f) <> foldMap (tV2 11) f) fit
  <> maybe mempty (tV2 12) st <> maybe mempty (tV2 13) en

lineOut :: PatLine -> Out
lineOut (PatLine a (V2 bx by) (V2 ox oy) ds) = tD 53 a <> tD 43 bx <> tD 44 by <> tD 45 ox <> tD 46 oy <> tI 79 (length ds) <> foldMap (tD 49) ds

dataOut :: HatchData -> Out
dataOut h =
  tV3 10 (hElevation h) <> maybe mempty (tV3 210) (hExtrusion h) <> tS 2 (hPatName h) <> tI 70 (hSolid h)
  <> tMaybeI 71 (hAssoc h) <> tI 91 (length (hPaths h)) <> foldMap pathOut (hPaths h)
  <> tMaybeI 75 (hStyle h) <> tMaybeI 76 (hPatType h) <> tMaybeD 52 (hAngle h) <> tMaybeD 41 (hScale h)
  <> tMaybeI 77 (hDouble h) <> maybe mempty (\l -> tI 78 (length l) <> foldMap lineOut l) (hLines h)
  <> tMaybeD 47 (hPixel h)
  <> maybe mempty (\s -> tI 98 (length s) <> foldMap (tV2 10) s) (hSeeds h)
  <> tTags (hGradient h)

hatchTags :: Hatch -> Out
hatchTags (Hatch d raw) = maybe (tTags raw) dataOut d

-- | What ezdxf shows of a hatch: the attributes, the kinds and sizes of the paths.
hatchDump :: Hatch -> [(B.ByteString, J)]
hatchDump (Hatch Nothing _) = []
hatchDump (Hatch (Just h) _) =
  [ ("elevation", jPoint3 (hElevation h)), ("pattern_name", JS (hPatName h)), ("solid_fill", JI (hSolid h)) ]
  ++ [("extrusion", jPoint3 e) | Just e <- [hExtrusion h]] ++ [("associative", JI a) | Just a <- [hAssoc h]]
  ++ [("hatch_style", JI a) | Just a <- [hStyle h]] ++ [("pattern_type", JI a) | Just a <- [hPatType h]]
  ++ [("pattern_angle", JD a) | Just a <- [hAngle h]] ++ [("pattern_scale", JD a) | Just a <- [hScale h]]
  ++ [("pattern_double", JI a) | Just a <- [hDouble h]] ++ [("pixel_size", JD a) | Just a <- [hPixel h]]
  ++ [ ("paths", JA (map pj (hPaths h))), ("pattern_lines", JI (maybe 0 length (hLines h)))
     , ("seeds", JA (map jPoint2 (maybe [] id (hSeeds h)))) ]
  where
    pj (PolyPath _ _ _ vs _) = JA [JS "polyline", JI (length vs)]
    pj (EdgePath _ es _) = JA [JS "edges", JI (length es)]

hatchCodec :: Codec Hatch
hatchCodec = codec (Hatch Nothing [])
  ([ mark "AcDbHatch"
   , custom "" codes (\c v r -> Just r { hdRaw = Tag c v : hdRaw r }) hatchTags (const Nothing) fin ]
   ++ [custom n [] (\_ _ _ -> Nothing) (const mempty) (\r -> lookup n (hatchDump r)) id | n <- names])
  where
    codes = [10, 20, 30, 11, 21, 40, 41, 42, 43, 44, 45, 46, 47, 49, 50, 51, 52, 53, 2, 63, 421]
            ++ [70 .. 79] ++ [91 .. 98] ++ [210, 220, 230, 330, 450, 451, 452, 453, 460, 461, 462, 463, 470]
    names = ["elevation", "extrusion", "pattern_name", "solid_fill", "associative", "hatch_style", "pattern_type"
            , "pattern_angle", "pattern_scale", "pattern_double", "pixel_size", "paths", "pattern_lines", "seeds"]
    fin r = let ts = reverse (hdRaw r) in case parseHatch ts of
      Just d -> Hatch (Just d) []
      Nothing -> Hatch Nothing ts

hatchCodecs :: [BodyCodec HatchBody]
hatchCodecs =
  [ entityCodec "HATCH" True hatchCodec BHatchE (\(BHatchE h) -> Just h) ]
