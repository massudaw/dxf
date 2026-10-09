{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | Text entities: TEXT, ATTRIB, ATTDEF, MTEXT, TOLERANCE, SHAPE.
-- Strings are the file's bytes (see "DXF.Encoding" to decode them).
module DXF.Entity.Text
  ( TxtBody (..), txtCodecs
  , TextData (..), Text (..), Attrib (..), MText (..), Tolerance (..), Shape (..)
  , mtextContent
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Json
import DXF.Number
import DXF.Tag
import DXF.Tag.Out
import DXF.Vec

data TxtBody
  = TText !Text
  | TAttrib !Attrib
  | TAttDef !Attrib
  | TMText !MText
  | TTolerance !Tolerance
  | TShape !Shape
  deriving (Eq, Show, Generic, NFData)

-- | What TEXT, ATTRIB and ATTDEF share.
data TextData = TextData
  { tdInsert    :: !V3                       -- ^ 10
  , tdHeight    :: !Double                   -- ^ 40
  , tdText      :: !B.ByteString             -- ^ 1
  , tdRotation  :: !(Maybe Double)           -- ^ 50, degrees
  , tdWidth     :: !(Maybe Double)           -- ^ 41, width factor
  , tdOblique   :: !(Maybe Double)           -- ^ 51, degrees
  , tdStyle     :: !(Maybe B.ByteString)     -- ^ 7
  , tdGenFlags  :: !(Maybe Int)              -- ^ 71, 2 mirrored in x, 4 mirrored in y
  , tdHAlign    :: !(Maybe Int)              -- ^ 72
  , tdAlign     :: !(Maybe V3)               -- ^ 11, second alignment point
  , tdThickness :: !(Maybe Double)           -- ^ 39
  , tdElevation :: !(Maybe Double)           -- ^ 38 (R12)
  , tdExtrusion :: !(Maybe V3)               -- ^ 210
  } deriving (Eq, Show, Generic, NFData)

blankTextData :: TextData
blankTextData = TextData v3zero 2.5 "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Text = Text
  { txData   :: !TextData
  , txValign :: !(Maybe Int)                 -- ^ 73
  } deriving (Eq, Show, Generic, NFData)

-- | ATTRIB and ATTDEF.
data Attrib = Attrib
  { atData      :: !TextData
  , atTag       :: !B.ByteString             -- ^ 2
  , atFlags     :: !Int                      -- ^ 70
  , atFieldLen  :: !(Maybe Int)              -- ^ 73
  , atValign    :: !(Maybe Int)              -- ^ 74
  , atLock      :: !(Maybe Int)              -- ^ 280 lock position
  , atVersion   :: !(Maybe Int)              -- ^ 280 before the tag (R2010)
  , atAttrType  :: !(Maybe Int)              -- ^ 71 in the attribute subclass (R2018)
  , atPrompt    :: !(Maybe B.ByteString)     -- ^ 3, ATTDEF only
  , atInAttr    :: !Bool                     -- ^ reading: past the attribute subclass marker
  } deriving (Eq, Show, Generic, NFData)

data MText = MText
  { mtInsert     :: !V3                      -- ^ 10
  , mtHeight     :: !Double                  -- ^ 40, character height
  , mtWidth      :: !(Maybe Double)          -- ^ 41, reference rectangle width
  , mtAttach     :: !(Maybe Int)             -- ^ 71
  , mtFlow       :: !(Maybe Int)             -- ^ 72
  , mtParts      :: [B.ByteString]           -- ^ 3 (chunks of 250) then 1, in file order; see 'mtextContent'
  , mtStyle      :: !(Maybe B.ByteString)    -- ^ 7
  , mtExtrusion  :: !(Maybe V3)              -- ^ 210
  , mtDirection  :: !(Maybe V3)              -- ^ 11
  , mtRectWidth  :: !(Maybe Double)          -- ^ 42
  , mtRectHeight :: !(Maybe Double)          -- ^ 43
  , mtRotation   :: !(Maybe Double)          -- ^ 50
  , mtSpacingStyle :: !(Maybe Int)           -- ^ 73
  , mtSpacing    :: !(Maybe Double)          -- ^ 44
  , mtDefHeight  :: !(Maybe Double)          -- ^ 46
  , mtBgFill     :: !(Maybe Int)             -- ^ 90
  , mtBgScale    :: !(Maybe Double)          -- ^ 45
  , mtBgColor    :: !(Maybe Int)             -- ^ 63
  , mtBgTrueColor :: !(Maybe Int)            -- ^ 421
  , mtBgColorName :: !(Maybe B.ByteString)   -- ^ 431
  , mtBgTransparency :: !(Maybe Int)         -- ^ 441
  } deriving (Eq, Show, Generic, NFData)

-- | The text of an MTEXT: its parts joined.
mtextContent :: MText -> B.ByteString
mtextContent = B.concat . mtParts

data Tolerance = Tolerance
  { tolStyle     :: !(Maybe B.ByteString)    -- ^ 3, dimension style
  , tolInsert    :: !V3                      -- ^ 10
  , tolText      :: !B.ByteString            -- ^ 1
  , tolExtrusion :: !(Maybe V3)              -- ^ 210
  , tolXAxis     :: !(Maybe V3)              -- ^ 11
  } deriving (Eq, Show, Generic, NFData)

data Shape = Shape
  { shInsert    :: !V3                       -- ^ 10
  , shSize      :: !Double                   -- ^ 40
  , shName      :: !B.ByteString             -- ^ 2
  , shRotation  :: !(Maybe Double)           -- ^ 50
  , shXScale    :: !(Maybe Double)           -- ^ 41
  , shOblique   :: !(Maybe Double)           -- ^ 51
  , shThickness :: !(Maybe Double)           -- ^ 39
  , shElevation :: !(Maybe Double)           -- ^ 38
  , shExtrusion :: !(Maybe V3)               -- ^ 210
  } deriving (Eq, Show, Generic, NFData)

-- | The fields of a text; the generation flags (71) are left out when
-- the owner reads 71 itself.
textFields :: Bool -> [F TextData]
textFields withFlags =
  [ mdbl "thickness" 39 (lens tdThickness (\r v -> r { tdThickness = v }))
  , pt3 "insert" 10 (lens tdInsert (\r v -> r { tdInsert = v }))
  , dbl "height" 40 (lens tdHeight (\r v -> r { tdHeight = v }))
  , str "text" 1 (lens tdText (\r v -> r { tdText = v }))
  , mdbl "rotation" 50 (lens tdRotation (\r v -> r { tdRotation = v }))
  , mdbl "width" 41 (lens tdWidth (\r v -> r { tdWidth = v }))
  , mdbl "oblique" 51 (lens tdOblique (\r v -> r { tdOblique = v }))
  , mstr "style" 7 (lens tdStyle (\r v -> r { tdStyle = v }))
  , mdbl "elevation" 38 (lens tdElevation (\r v -> r { tdElevation = v })) ]
  ++ [ mint "text_generation_flag" 71 (lens tdGenFlags (\r v -> r { tdGenFlags = v })) | withFlags ]
  ++
  [ mint "halign" 72 (lens tdHAlign (\r v -> r { tdHAlign = v }))
  , mpt3 "align_point" 11 (lens tdAlign (\r v -> r { tdAlign = v }))
  , mpt3 "extrusion" 210 (lens tdExtrusion (\r v -> r { tdExtrusion = v })) ]

textCodec :: Codec Text
textCodec = codec (Text blankTextData Nothing)
  ([mark "AcDbText"] ++ map (under txData (\a r -> r { txData = a })) (textFields True)
   ++ [mark "AcDbText", mint "valign" 73 (lens txValign (\r v -> r { txValign = v }))])

-- | ATTRIB / ATTDEF.  Code 71 means the generation flags in the text
-- subclass and the attribute type in the attribute subclass; 280 is the
-- lock position, preceded by a version when there are two.
attribCodec :: B.ByteString -> Codec Attrib
attribCodec sub = codec (Attrib blankTextData "" 0 Nothing Nothing Nothing Nothing Nothing Nothing False)
  ([mark "AcDbText"] ++ map (under atData (\a r -> r { atData = a })) (textFields False)
   ++ [ custom "" [71] rd71 (const mempty) (const Nothing) id
      , custom "text_generation_flag" [] (\_ _ _ -> Nothing) (maybe mempty (tI 71) . tdGenFlags . atData) (fmap JI . tdGenFlags . atData) id
      , markS sub (\r -> r { atInAttr = True })
      , custom "" [280] rd280 (maybe mempty (tI 280) . atVersion) (const Nothing) id
      , mstr "prompt" 3 (lens atPrompt (\r v -> r { atPrompt = v }))
      , str "tag" 2 (lens atTag (\r v -> r { atTag = v }))
      , int "flags" 70 (lens atFlags (\r v -> r { atFlags = v }))
      , mint "field_length" 73 (lens atFieldLen (\r v -> r { atFieldLen = v }))
      , mint "valign" 74 (lens atValign (\r v -> r { atValign = v }))
      , custom "lock_position" [] (\_ _ _ -> Nothing) (maybe mempty (tI 280) . atLock) (fmap JI . atLock) id
      , custom "attribute_type" [] (\_ _ _ -> Nothing) (maybe mempty (tI 71) . atAttrType) (fmap JI . atAttrType) id ])
  where
    rd71 _ v r = readInt v >>= \n ->
      if atInAttr r then (if atAttrType r == Nothing then Just r { atAttrType = Just n } else Nothing)
      else (if tdGenFlags (atData r) == Nothing then Just r { atData = (atData r) { tdGenFlags = Just n } } else Nothing)
    rd280 _ v r = readInt v >>= \n -> case atLock r of
      Nothing -> Just r { atLock = Just n }
      Just old | atVersion r == Nothing -> Just r { atVersion = Just old, atLock = Just n }
      _ -> Nothing

mtextCodec :: Codec MText
mtextCodec = codec (MText v3zero 2.5 Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  [ mark "AcDbMText"
  , pt3 "insert" 10 (lens mtInsert (\r v -> r { mtInsert = v }))
  , dbl "char_height" 40 (lens mtHeight (\r v -> r { mtHeight = v }))
  , mdbl "width" 41 (lens mtWidth (\r v -> r { mtWidth = v }))
  , mint "attachment_point" 71 (lens mtAttach (\r v -> r { mtAttach = v }))
  , mint "flow_direction" 72 (lens mtFlow (\r v -> r { mtFlow = v }))
  , custom "text" [1, 3] (\_ v r -> Just r { mtParts = v : mtParts r }) (\r -> parts (mtParts r))
      (\r -> Just (JS (B.concat (mtParts r)))) (\r -> r { mtParts = reverse (mtParts r) })
  , mstr "style" 7 (lens mtStyle (\r v -> r { mtStyle = v }))
  , mpt3 "extrusion" 210 (lens mtExtrusion (\r v -> r { mtExtrusion = v }))
  , mpt3 "text_direction" 11 (lens mtDirection (\r v -> r { mtDirection = v }))
  , mdbl "rect_width" 42 (lens mtRectWidth (\r v -> r { mtRectWidth = v }))
  , mdbl "rect_height" 43 (lens mtRectHeight (\r v -> r { mtRectHeight = v }))
  , mdbl "rotation" 50 (lens mtRotation (\r v -> r { mtRotation = v }))
  , mint "line_spacing_style" 73 (lens mtSpacingStyle (\r v -> r { mtSpacingStyle = v }))
  , mdbl "line_spacing_factor" 44 (lens mtSpacing (\r v -> r { mtSpacing = v }))
  , mdbl "defined_height" 46 (lens mtDefHeight (\r v -> r { mtDefHeight = v }))
  , mint "bg_fill" 90 (lens mtBgFill (\r v -> r { mtBgFill = v }))
  , mdbl "box_fill_scale" 45 (lens mtBgScale (\r v -> r { mtBgScale = v }))
  , mint "bg_fill_color" 63 (lens mtBgColor (\r v -> r { mtBgColor = v }))
  , mint "bg_fill_true_color" 421 (lens mtBgTrueColor (\r v -> r { mtBgTrueColor = v }))
  , mstr "bg_fill_color_name" 431 (lens mtBgColorName (\r v -> r { mtBgColorName = v }))
  , mint "bg_fill_transparency" 441 (lens mtBgTransparency (\r v -> r { mtBgTransparency = v })) ]
  where
    -- the parts as they were: all but the last under 3, the last under 1
    parts [] = tS 1 ""
    parts ps = foldMap (tS 3) (init ps) <> tS 1 (last ps)

toleranceCodec :: Codec Tolerance
toleranceCodec = codec (Tolerance Nothing v3zero "" Nothing Nothing)
  [ mark "AcDbFcf"
  , mstr "dimstyle" 3 (lens tolStyle (\r v -> r { tolStyle = v }))
  , pt3 "insert" 10 (lens tolInsert (\r v -> r { tolInsert = v }))
  , str "content" 1 (lens tolText (\r v -> r { tolText = v }))
  , mpt3 "extrusion" 210 (lens tolExtrusion (\r v -> r { tolExtrusion = v }))
  , mpt3 "x_axis_vector" 11 (lens tolXAxis (\r v -> r { tolXAxis = v })) ]

shapeCodec :: Codec Shape
shapeCodec = codec (Shape v3zero 1 "" Nothing Nothing Nothing Nothing Nothing Nothing)
  [ mark "AcDbShape"
  , mdbl "thickness" 39 (lens shThickness (\r v -> r { shThickness = v }))
  , pt3 "insert" 10 (lens shInsert (\r v -> r { shInsert = v }))
  , dbl "size" 40 (lens shSize (\r v -> r { shSize = v }))
  , str "name" 2 (lens shName (\r v -> r { shName = v }))
  , mdbl "rotation" 50 (lens shRotation (\r v -> r { shRotation = v }))
  , mdbl "xscale" 41 (lens shXScale (\r v -> r { shXScale = v }))
  , mdbl "oblique" 51 (lens shOblique (\r v -> r { shOblique = v }))
  , mdbl "elevation" 38 (lens shElevation (\r v -> r { shElevation = v }))
  , mpt3 "extrusion" 210 (lens shExtrusion (\r v -> r { shExtrusion = v })) ]

txtCodecs :: [BodyCodec TxtBody]
txtCodecs =
  [ entityCodec "TEXT" True textCodec TText (\g -> case g of TText r -> Just r; _ -> Nothing)
  , entityCodec "ATTRIB" True (attribCodec "AcDbAttribute") TAttrib (\g -> case g of TAttrib r -> Just r; _ -> Nothing)
  , entityCodec "ATTDEF" True (attribCodec "AcDbAttributeDefinition") TAttDef (\g -> case g of TAttDef r -> Just r; _ -> Nothing)
  , entityCodec "MTEXT" True mtextCodec TMText (\g -> case g of TMText r -> Just r; _ -> Nothing)
  , entityCodec "TOLERANCE" True toleranceCodec TTolerance (\g -> case g of TTolerance r -> Just r; _ -> Nothing)
  , entityCodec "SHAPE" True shapeCodec TShape (\g -> case g of TShape r -> Just r; _ -> Nothing) ]

_unusedImports :: (Tag, Maybe Double)
_unusedImports = (Tag 0 "", readDouble "")
