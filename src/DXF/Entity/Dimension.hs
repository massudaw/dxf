{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | DIMENSION (every kind: linear, aligned, angular, diameter, radius,
-- ordinate; the kind is the low bits of 'dmType') and LEADER.  The
-- geometry block, the definition points and the measurement are typed; the
-- subclass markers follow the kind.  ARC_DIMENSION, the large radial
-- dimension and MLEADER are kept as tags.
module DXF.Entity.Dimension
  ( DimBody (..), dimCodecs
  , Dimension (..), Leader (..), dimSubclass
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Json
import DXF.Number
import DXF.Tag.Out
import DXF.Vec

data DimBody
  = DDimension !Dimension
  | DLeader !Leader
  deriving (Eq, Show, Generic, NFData)

data Dimension = Dimension
  { dmVersion  :: !(Maybe Int)            -- ^ 280
  , dmGeometry :: !(Maybe B.ByteString)   -- ^ 2, the anonymous block holding the picture
  , dmStyle    :: !(Maybe B.ByteString)   -- ^ 3
  , dmDefPoint :: !V3                     -- ^ 10
  , dmTextMid  :: !(Maybe V3)             -- ^ 11
  , dmInsert   :: !(Maybe V3)             -- ^ 12
  , dmType     :: !Int                    -- ^ 70: 0 linear, 1 aligned, 2 angular, 3 diameter, 4 radius, 5 angular 3 point, 6 ordinate; +32 block reference, +64 ordinate x, +128 text position set
  , dmAttach   :: !(Maybe Int)            -- ^ 71
  , dmSpacingStyle :: !(Maybe Int)        -- ^ 72
  , dmSpacingFactor :: !(Maybe Double)    -- ^ 41
  , dmMeasurement :: !(Maybe Double)      -- ^ 42
  , dmFlip1, dmFlip2 :: !(Maybe Int)      -- ^ 74, 75
  , dmText     :: !(Maybe B.ByteString)   -- ^ 1
  , dmOblique  :: !(Maybe Double)         -- ^ 52
  , dmTextRotation :: !(Maybe Double)     -- ^ 53
  , dmHorizontal :: !(Maybe Double)       -- ^ 51
  , dmExtrusion :: !(Maybe V3)            -- ^ 210
  , dmPoint2, dmPoint3 :: !(Maybe V3)     -- ^ 13, 14
  , dmAngle    :: !(Maybe Double)         -- ^ 50
  , dmPoint4   :: !(Maybe V3)             -- ^ 15
  , dmLeaderLength :: !(Maybe Double)     -- ^ 40
  , dmPoint5   :: !(Maybe V3)             -- ^ 16
  } deriving (Eq, Show, Generic, NFData)

data Leader = Leader
  { ldStyle :: !(Maybe B.ByteString)      -- ^ 3
  , ldArrow :: !(Maybe Int)               -- ^ 71
  , ldPathType :: !(Maybe Int)            -- ^ 72
  , ldAnnotation :: !(Maybe Int)          -- ^ 73
  , ldHookDir :: !(Maybe Int)             -- ^ 74
  , ldHook :: !(Maybe Int)                -- ^ 75
  , ldTextHeight, ldTextWidth :: !(Maybe Double) -- ^ 40, 41
  , ldVertices :: [V3]                    -- ^ 10 (76 counts them)
  , ldColor :: !(Maybe Int)               -- ^ 77
  , ldHandle :: !(Maybe B.ByteString)     -- ^ 340
  , ldNormal, ldHorizontal, ldBlockOffset, ldAnnotOffset :: !(Maybe V3) -- ^ 210, 211, 212, 213
  } deriving (Eq, Show, Generic, NFData)

-- | The subclass marker(s) a dimension of this type has.
dimSubclass :: Int -> [B.ByteString]
dimSubclass t = case t `mod` 8 of
  0 -> ["AcDbAlignedDimension", "AcDbRotatedDimension"]
  1 -> ["AcDbAlignedDimension"]
  2 -> ["AcDbAngular2LineDimension"]
  3 -> ["AcDbDiametricDimension"]
  4 -> ["AcDbRadialDimension"]
  5 -> ["AcDbAngular3PointDimension"]
  _ -> ["AcDbOrdinateDimension"]

dimCodec :: Codec Dimension
dimCodec = codec (Dimension Nothing Nothing Nothing v3zero Nothing Nothing 0 Nothing Nothing Nothing Nothing Nothing Nothing
                  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  ([ mark "AcDbDimension" ]
   ++ map markR ["AcDbAlignedDimension", "AcDbRotatedDimension", "AcDbAngular2LineDimension", "AcDbDiametricDimension"
                , "AcDbRadialDimension", "AcDbAngular3PointDimension", "AcDbOrdinateDimension"]
   ++
  [ mint "version" 280 (lens dmVersion (\r v -> r { dmVersion = v }))
  , mstr "geometry" 2 (lens dmGeometry (\r v -> r { dmGeometry = v }))
  , mstr "dimstyle" 3 (lens dmStyle (\r v -> r { dmStyle = v }))
  , pt3 "defpoint" 10 (lens dmDefPoint (\r v -> r { dmDefPoint = v }))
  , mpt3 "text_midpoint" 11 (lens dmTextMid (\r v -> r { dmTextMid = v }))
  , mpt3 "insert" 12 (lens dmInsert (\r v -> r { dmInsert = v }))
  , int "dimtype" 70 (lens dmType (\r v -> r { dmType = v }))
  , mint "attachment_point" 71 (lens dmAttach (\r v -> r { dmAttach = v }))
  , mint "line_spacing_style" 72 (lens dmSpacingStyle (\r v -> r { dmSpacingStyle = v }))
  , mdbl "line_spacing_factor" 41 (lens dmSpacingFactor (\r v -> r { dmSpacingFactor = v }))
  , mdbl "actual_measurement" 42 (lens dmMeasurement (\r v -> r { dmMeasurement = v }))
  , mint "flip_arrow_1" 74 (lens dmFlip1 (\r v -> r { dmFlip1 = v }))
  , mint "flip_arrow_2" 75 (lens dmFlip2 (\r v -> r { dmFlip2 = v }))
  , mstr "text" 1 (lens dmText (\r v -> r { dmText = v }))
  , mdbl "oblique_angle" 52 (lens dmOblique (\r v -> r { dmOblique = v }))
  , mdbl "text_rotation" 53 (lens dmTextRotation (\r v -> r { dmTextRotation = v }))
  , mdbl "horizontal_direction" 51 (lens dmHorizontal (\r v -> r { dmHorizontal = v }))
  , mpt3 "extrusion" 210 (lens dmExtrusion (\r v -> r { dmExtrusion = v }))
  , custom "" [] (\_ _ _ -> Nothing) (tMarkers . take 1 . dimSubclass . dmType) (const Nothing) id
  , mpt3 "defpoint2" 13 (lens dmPoint2 (\r v -> r { dmPoint2 = v }))
  , mpt3 "defpoint3" 14 (lens dmPoint3 (\r v -> r { dmPoint3 = v }))
  , mdbl "angle" 50 (lens dmAngle (\r v -> r { dmAngle = v }))
  , mpt3 "defpoint4" 15 (lens dmPoint4 (\r v -> r { dmPoint4 = v }))
  , mdbl "leader_length" 40 (lens dmLeaderLength (\r v -> r { dmLeaderLength = v }))
  , mpt3 "defpoint5" 16 (lens dmPoint5 (\r v -> r { dmPoint5 = v }))
  , custom "" [] (\_ _ _ -> Nothing) (tMarkers . drop 1 . dimSubclass . dmType) (const Nothing) id ])
  where tMarkers = foldMap tMarker

leaderCodec :: Codec Leader
leaderCodec = codec (Leader Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing)
  [ mark "AcDbLeader"
  , mstr "dimstyle" 3 (lens ldStyle (\r v -> r { ldStyle = v }))
  , mint "has_arrowhead" 71 (lens ldArrow (\r v -> r { ldArrow = v }))
  , mint "path_type" 72 (lens ldPathType (\r v -> r { ldPathType = v }))
  , mint "annotation_type" 73 (lens ldAnnotation (\r v -> r { ldAnnotation = v }))
  , mint "hookline_direction" 74 (lens ldHookDir (\r v -> r { ldHookDir = v }))
  , mint "has_hookline" 75 (lens ldHook (\r v -> r { ldHook = v }))
  , mdbl "text_height" 40 (lens ldTextHeight (\r v -> r { ldTextHeight = v }))
  , mdbl "text_width" 41 (lens ldTextWidth (\r v -> r { ldTextWidth = v }))
  , custom "" [76] (\_ _ r -> Just r) (tI 76 . length . ldVertices) (const Nothing) id
  , pts3 "vertices" 10 (lens ldVertices (\r v -> r { ldVertices = v }))
  , mint "block_color" 77 (lens ldColor (\r v -> r { ldColor = v }))
  , mstr "annotation_handle" 340 (lens ldHandle (\r v -> r { ldHandle = v }))
  , mpt3 "normal_vector" 210 (lens ldNormal (\r v -> r { ldNormal = v }))
  , mpt3 "horizontal_direction" 211 (lens ldHorizontal (\r v -> r { ldHorizontal = v }))
  , mpt3 "leader_offset_block_ref" 212 (lens ldBlockOffset (\r v -> r { ldBlockOffset = v }))
  , mpt3 "leader_offset_annotation_placement" 213 (lens ldAnnotOffset (\r v -> r { ldAnnotOffset = v })) ]

dimCodecs :: [BodyCodec DimBody]
dimCodecs =
  [ entityCodec "DIMENSION" True dimCodec DDimension (\g -> case g of DDimension r -> Just r; _ -> Nothing)
  , entityCodec "LEADER" True leaderCodec DLeader (\g -> case g of DLeader r -> Just r; _ -> Nothing) ]
