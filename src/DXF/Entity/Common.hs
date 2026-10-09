{-# LANGUAGE BangPatterns, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | What every entity, table entry and object has: handle, owner,
-- application data, and (for graphical entities) layer, linetype, color
-- and the rest of the @AcDbEntity@ subclass; extended data at the end.
--
-- 'splitTags' takes the tags of an entity (after the @0 TYPE@ tag) apart
-- into this and the tags that are the entity's own; 'commonHead' and
-- 'commonTail' write it back around them.
module DXF.Entity.Common
  ( Common (..), More (..), blankCommon, blankMore
  , splitTags, commonHead, commonTail, commonDump
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Json
import DXF.Number
import DXF.Tag
import DXF.Tag.Out

-- | The @AcDbEntity@ attributes that are rarely set.
data More = More
  { mInvisible    :: !(Maybe Int)           -- ^ 60
  , mTrueColor    :: !(Maybe Int)           -- ^ 420
  , mColorName    :: !(Maybe B.ByteString)  -- ^ 430
  , mTransparency :: !(Maybe Int)           -- ^ 440
  , mShadow       :: !(Maybe Int)           -- ^ 284
  , mMaterial     :: !(Maybe B.ByteString)  -- ^ 347
  , mVisualStyle  :: !(Maybe B.ByteString)  -- ^ 348
  , mPlotStyleEnum :: !(Maybe Int)          -- ^ 380
  , mPlotStyle    :: !(Maybe B.ByteString)  -- ^ 390
  } deriving (Eq, Show, Generic, NFData)

blankMore :: More
blankMore = More Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Common = Common
  { cHandle     :: !(Maybe B.ByteString)  -- ^ 5, hex text
  , cOwner      :: !(Maybe B.ByteString)  -- ^ 330 before the first subclass marker
  , cAppData    :: [[Tag]]                -- ^ @102 {...@ groups (reactors, xdictionary), with their 102 tags
  , cLayer      :: !(Maybe B.ByteString)  -- ^ 8
  , cLinetype   :: !(Maybe B.ByteString)  -- ^ 6
  , cColor      :: !(Maybe Int)           -- ^ 62
  , cPaperspace :: !(Maybe Int)           -- ^ 67
  , cLineweight :: !(Maybe Int)           -- ^ 370
  , cLtScale    :: !(Maybe Double)        -- ^ 48
  , cMore       :: !More
  , cXData      :: [Tag]                  -- ^ from the first 1001 tag on
  , cExtra      :: [Tag]                  -- ^ tags nothing understood, in order
  } deriving (Eq, Show, Generic, NFData)

blankCommon :: Common
blankCommon = Common Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing blankMore [] []

data Phase = Pre | Ent | Bod

-- | The common part and the entity's own tags.  The flag says whether the
-- entity is graphical (has the @AcDbEntity@ attributes); R12 files, which
-- have no subclass markers, are read the same way.
splitTags :: Bool -> [Tag] -> (Common, [Tag])
splitTags graphic = go Pre blankCommon []
  where
    done c body xd = (c { cAppData = reverse (cAppData c), cXData = xd }, reverse body)
    go !_ c body [] = done c body []
    go ph c body ts@(t@(Tag k v) : rest)
      | k == 1001 = done c body ts
      | otherwise = case ph of
          Bod -> go Bod c (t : body) rest
          _ | k == 100 -> if graphic && v == "AcDbEntity" && isPre ph then go Ent c body rest
                           else go Bod c (t : body) rest
            | Pre <- ph, k == 5, Nothing <- cHandle c -> go ph c { cHandle = Just v } body rest
            | Pre <- ph, k == 330, Nothing <- cOwner c -> go ph c { cOwner = Just v } body rest
            | Pre <- ph, k == 102, "{" `B.isPrefixOf` v ->
                let (grp, rest') = group102 ts in go ph c { cAppData = grp : cAppData c } body rest'
            | graphic, Just c' <- field k v c -> go ph c' body rest
            | otherwise -> go ph c (t : body) rest
    isPre Pre = True
    isPre _ = False
    group102 (a : r) = let (g, r') = break (\(Tag k _) -> k == 102) r
                       in case r' of (e : r'') -> (a : g ++ [e], r'')
                                     [] -> (a : g, [])
    group102 [] = ([], [])

-- | A graphical attribute, if the tag is one that is not yet set.
field :: Int -> B.ByteString -> Common -> Maybe Common
field k v c = case k of
  8 | Nothing <- cLayer c -> Just c { cLayer = Just v }
  6 | Nothing <- cLinetype c -> Just c { cLinetype = Just v }
  62 | Nothing <- cColor c, Just n <- readInt v -> Just c { cColor = Just n }
  67 | Nothing <- cPaperspace c, Just n <- readInt v -> Just c { cPaperspace = Just n }
  370 | Nothing <- cLineweight c, Just n <- readInt v -> Just c { cLineweight = Just n }
  48 | Nothing <- cLtScale c, Just d <- readDouble v -> Just c { cLtScale = Just d }
  60 | Nothing <- mInvisible m, Just n <- readInt v -> more m { mInvisible = Just n }
  420 | Nothing <- mTrueColor m, Just n <- readInt v -> more m { mTrueColor = Just n }
  430 | Nothing <- mColorName m -> more m { mColorName = Just v }
  440 | Nothing <- mTransparency m, Just n <- readInt v -> more m { mTransparency = Just n }
  284 | Nothing <- mShadow m, Just n <- readInt v -> more m { mShadow = Just n }
  347 | Nothing <- mMaterial m -> more m { mMaterial = Just v }
  348 | Nothing <- mVisualStyle m -> more m { mVisualStyle = Just v }
  380 | Nothing <- mPlotStyleEnum m, Just n <- readInt v -> more m { mPlotStyleEnum = Just n }
  390 | Nothing <- mPlotStyle m -> more m { mPlotStyle = Just v }
  _ -> Nothing
  where m = cMore c
        more m' = Just c { cMore = m' }

-- | @0 TYPE@, handle, application data, owner, and for graphical
-- entities the @AcDbEntity@ subclass.
commonHead :: Bool -> B.ByteString -> Common -> Out
commonHead graphic ty c =
  tS 0 ty <> tMaybeS 5 (cHandle c) <> foldMap tTags (cAppData c) <> tMaybeS 330 (cOwner c)
  <> (if graphic then ent else mempty)
  where
    m = cMore c
    ent = tMarker "AcDbEntity" <> tMaybeI 67 (cPaperspace c) <> tMaybeS 8 (cLayer c)
      <> tMaybeS 6 (cLinetype c) <> tMaybeI 62 (cColor c) <> tMaybeI 370 (cLineweight c)
      <> tMaybeD 48 (cLtScale c) <> tMaybeI 60 (mInvisible m) <> tMaybeI 420 (mTrueColor m)
      <> tMaybeS 430 (mColorName m) <> tMaybeI 440 (mTransparency m) <> tMaybeI 284 (mShadow m)
      <> tMaybeS 347 (mMaterial m) <> tMaybeS 348 (mVisualStyle m) <> tMaybeI 380 (mPlotStyleEnum m)
      <> tMaybeS 390 (mPlotStyle m)

-- | What nothing understood, then the extended data.
commonTail :: Common -> Out
commonTail c = tTags (cExtra c) <> tTags (cXData c)

-- | The attributes, named as ezdxf names them.
commonDump :: Common -> [(B.ByteString, J)]
commonDump c = concat
  [ s "handle" (cHandle c), s "owner" (cOwner c), s "layer" (cLayer c), s "linetype" (cLinetype c)
  , i "color" (cColor c), i "paperspace" (cPaperspace c), i "lineweight" (cLineweight c)
  , [("ltscale", JD d) | Just d <- [cLtScale c]]
  , i "invisible" (mInvisible m), i "true_color" (mTrueColor m), s "color_name" (mColorName m)
  , i "transparency" (mTransparency m), i "shadow_mode" (mShadow m), s "material_handle" (mMaterial m)
  , s "visualstyle_handle" (mVisualStyle m), i "plotstyle_enum" (mPlotStyleEnum m), s "plotstyle_handle" (mPlotStyle m) ]
  where
    m = cMore c
    s n v = [(n, JS x) | Just x <- [v]]
    i n v = [(n, JI x) | Just x <- [v]]
