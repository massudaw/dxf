{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module DXF.Entity.Geometry
  ( GeoBody (..), geoCodecs
  , Line (..), Point (..), Circle (..), Arc (..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Vec

-- | The simple geometric entities.  Optional attributes are 'Maybe': set
-- means present in the file (and written), as the file had it.
data GeoBody
  = GLine !Line
  | GPoint !Point
  | GCircle !Circle
  | GArc !Arc
  deriving (Eq, Show, Generic, NFData)

data Line = Line
  { lineStart     :: !V3
  , lineEnd       :: !V3
  , lineThickness :: !(Maybe Double)
  , lineExtrusion :: !(Maybe V3)
  } deriving (Eq, Show, Generic, NFData)

data Point = Point
  { pointLocation  :: !V3
  , pointThickness :: !(Maybe Double)
  , pointExtrusion :: !(Maybe V3)
  , pointAngle     :: !(Maybe Double)   -- ^ rotation of the point's coordinate system
  } deriving (Eq, Show, Generic, NFData)

data Circle = Circle
  { circleCenter    :: !V3
  , circleRadius    :: !Double
  , circleThickness :: !(Maybe Double)
  , circleExtrusion :: !(Maybe V3)
  } deriving (Eq, Show, Generic, NFData)

data Arc = Arc
  { arcCenter     :: !V3
  , arcRadius     :: !Double
  , arcStartAngle :: !Double
  , arcEndAngle   :: !Double
  , arcThickness  :: !(Maybe Double)
  , arcExtrusion  :: !(Maybe V3)
  } deriving (Eq, Show, Generic, NFData)

lineCodec :: Codec Line
lineCodec = codec (Line v3zero v3zero Nothing Nothing)
  [ mark "AcDbLine"
  , pt3 "start" 10 (lens lineStart (\r v -> r { lineStart = v }))
  , pt3 "end" 11 (lens lineEnd (\r v -> r { lineEnd = v }))
  , mdbl "thickness" 39 (lens lineThickness (\r v -> r { lineThickness = v }))
  , mpt3 "extrusion" 210 (lens lineExtrusion (\r v -> r { lineExtrusion = v })) ]

pointCodec :: Codec Point
pointCodec = codec (Point v3zero Nothing Nothing Nothing)
  [ mark "AcDbPoint"
  , pt3 "location" 10 (lens pointLocation (\r v -> r { pointLocation = v }))
  , mdbl "thickness" 39 (lens pointThickness (\r v -> r { pointThickness = v }))
  , mpt3 "extrusion" 210 (lens pointExtrusion (\r v -> r { pointExtrusion = v }))
  , mdbl "angle" 50 (lens pointAngle (\r v -> r { pointAngle = v })) ]

circleCodec :: Codec Circle
circleCodec = codec (Circle v3zero 1 Nothing Nothing)
  [ mark "AcDbCircle"
  , pt3 "center" 10 (lens circleCenter (\r v -> r { circleCenter = v }))
  , dbl "radius" 40 (lens circleRadius (\r v -> r { circleRadius = v }))
  , mdbl "thickness" 39 (lens circleThickness (\r v -> r { circleThickness = v }))
  , mpt3 "extrusion" 210 (lens circleExtrusion (\r v -> r { circleExtrusion = v })) ]

arcCodec :: Codec Arc
arcCodec = codec (Arc v3zero 1 0 360 Nothing Nothing)
  [ mark "AcDbCircle"
  , pt3 "center" 10 (lens arcCenter (\r v -> r { arcCenter = v }))
  , dbl "radius" 40 (lens arcRadius (\r v -> r { arcRadius = v }))
  , mdbl "thickness" 39 (lens arcThickness (\r v -> r { arcThickness = v }))
  , mpt3 "extrusion" 210 (lens arcExtrusion (\r v -> r { arcExtrusion = v }))
  , mark "AcDbArc"
  , dbl "start_angle" 50 (lens arcStartAngle (\r v -> r { arcStartAngle = v }))
  , dbl "end_angle" 51 (lens arcEndAngle (\r v -> r { arcEndAngle = v })) ]

geoCodecs :: [BodyCodec GeoBody]
geoCodecs =
  [ entityCodec "LINE" True lineCodec GLine (\g -> case g of GLine r -> Just r; _ -> Nothing)
  , entityCodec "POINT" True pointCodec GPoint (\g -> case g of GPoint r -> Just r; _ -> Nothing)
  , entityCodec "CIRCLE" True circleCodec GCircle (\g -> case g of GCircle r -> Just r; _ -> Nothing)
  , entityCodec "ARC" True arcCodec GArc (\g -> case g of GArc r -> Just r; _ -> Nothing)
  ]
