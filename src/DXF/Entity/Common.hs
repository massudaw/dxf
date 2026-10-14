{-# LANGUAGE BangPatterns, DataKinds, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
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
  , plainHead, headB, headOne
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import GHC.Generics (Generic)
import DXF.Json
import DXF.Number
import DXF.Tag
import DXF.Tag.Out
import DXF.Tag.Prim
import Data.ByteString.Builder.Prim (primBounded)
import LinBuf.Builder (asBoundedPrim)
import LinBuf.Fixed (Sized (..))
import qualified LinBuf.Fixed as F

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
            | Pre <- ph, k == 5 || k == 105, Nothing <- cHandle c -> go ph c { cHandle = Just v } body rest
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
commonHead graphic ty c f
  | not (fmtBinary f), plainHead ty c =
      headOne f graphic ty c
  | otherwise = general
  where
    general =
      tS 0 ty f <> tMaybeS (handleCode ty) (cHandle c) f <> foldMap tTags (cAppData c) f <> tMaybeS 330 (cOwner c) f
      <> (if graphic then ent else mempty)
    m = cMore c
    ent = tMarker "AcDbEntity" f <> tMaybeI 67 (cPaperspace c) f <> tMaybeS 8 (cLayer c) f
      <> tMaybeS 6 (cLinetype c) f <> tMaybeI 62 (cColor c) f <> tMaybeI 370 (cLineweight c) f
      <> tMaybeD 48 (cLtScale c) f <> tMaybeI 60 (mInvisible m) f <> tMaybeI 420 (mTrueColor m) f
      <> tMaybeS 430 (mColorName m) f <> tMaybeI 440 (mTransparency m) f <> tMaybeI 284 (mShadow m) f
      <> tMaybeS 347 (mMaterial m) f <> tMaybeS 348 (mVisualStyle m) f <> tMaybeI 380 (mPlotStyleEnum m) f
      <> tMaybeS 390 (mPlotStyle m) f

-- | The group code of an entity's handle: 5, but a DIMSTYLE has 105.
handleCode :: B.ByteString -> Int
handleCode ty = if ty == "DIMSTYLE" then 105 else 5

-- | The usual head: no application data, none of the rare attributes,
-- strings short and numbers plain, so one primitive writes it all.
plainHead :: B.ByteString -> Common -> Bool
plainHead ty c = null (cAppData c) && cMore c == blankMore && shortBytes ty
  && sb (cHandle c) && sb (cOwner c) && sb (cLayer c) && sb (cLinetype c)
  && maybe True plainDouble (cLtScale c)
  where sb = maybe True shortBytes

-- | The head of a 'plainHead' as one linbuf 'F.Bounded': @0 TYPE@, handle, owner, and for a graphical
-- entity the @AcDbEntity@ subclass (not in R12) with its usual attributes, each there or not by 'F.choose'.
headB :: Bool -> Bool -> Bool -> Bool -> B.ByteString -> Common -> F.Bounded 746
headB pad crlf r12 graphic ty c =
  let !hc = handleCode ty in
  bytesTagB pad crlf 0 ty <+> optBytesB pad crlf hc (cHandle c) <+> optBytesB pad crlf 330 (cOwner c)
    <+> F.choose graphic
          (F.choose r12 (F.widen none) (bytesTagB pad crlf 100 "AcDbEntity")
            <+> optIntB pad crlf 67 (cPaperspace c) <+> optBytesB pad crlf 8 (cLayer c)
            <+> optBytesB pad crlf 6 (cLinetype c) <+> optIntB pad crlf 62 (cColor c)
            <+> optIntB pad crlf 370 (cLineweight c) <+> optDoubleB pad crlf 48 (cLtScale c))
          (F.widen none)
{-# INLINE headB #-}

-- | The head of a 'plainHead', run as one primitive.
headOne :: Fmt -> Bool -> B.ByteString -> Common -> BB.Builder
headOne f graphic ty =
  let !pad = fmtPad f; !crlf = fmtCRLF f; !r12 = fmtR12 f
  in primBounded (asBoundedPrim (headB pad crlf r12 graphic ty))
{-# NOINLINE headOne #-}

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
