{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module DXF.Entity.Geometry
  ( GeoBody (..), geoCodecs
  , Line (..), Point (..), Circle (..), Arc (..), Ellipse (..), Quad (..), Face3d (..), XLine (..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Tag.Out
import DXF.Vec

-- | The simple geometric entities.  Optional attributes are 'Maybe': set
-- means present in the file (and written), as the file had it.
data GeoBody
  = GLine !Line
  | GPoint !Point
  | GCircle !Circle
  | GArc !Arc
  | GEllipse !Ellipse
  | GSolid !Quad
  | GTrace !Quad
  | G3dFace !Face3d
  | GRay !XLine
  | GXLine !XLine
  deriving (Eq, Show, Generic, NFData)

data Line = Line
  { lineStart     :: {-# UNPACK #-} !V3
  , lineEnd       :: {-# UNPACK #-} !V3
  , lineThickness :: !(Maybe Double)
  , lineExtrusion :: !(Maybe V3)
  } deriving (Eq, Show, Generic, NFData)

data Point = Point
  { pointLocation  :: {-# UNPACK #-} !V3
  , pointThickness :: !(Maybe Double)
  , pointExtrusion :: !(Maybe V3)
  , pointAngle     :: !(Maybe Double)   -- ^ rotation of the point's coordinate system
  } deriving (Eq, Show, Generic, NFData)

data Circle = Circle
  { circleCenter    :: {-# UNPACK #-} !V3
  , circleRadius    :: !Double
  , circleThickness :: !(Maybe Double)
  , circleExtrusion :: !(Maybe V3)
  } deriving (Eq, Show, Generic, NFData)

data Arc = Arc
  { arcCenter     :: {-# UNPACK #-} !V3
  , arcRadius     :: !Double
  , arcStartAngle :: !Double
  , arcEndAngle   :: !Double
  , arcThickness  :: !(Maybe Double)
  , arcExtrusion  :: !(Maybe V3)
  } deriving (Eq, Show, Generic, NFData)

data Ellipse = Ellipse
  { elCenter    :: {-# UNPACK #-} !V3
  , elMajorAxis :: {-# UNPACK #-} !V3             -- ^ endpoint of the major axis relative to the center
  , elRatio     :: !Double         -- ^ minor to major
  , elStartParam :: !Double        -- ^ radians
  , elEndParam  :: !Double
  , elExtrusion :: !(Maybe V3)
  } deriving (Eq, Show, Generic, NFData)

-- | SOLID and TRACE: four corners (the third and fourth are equal for a triangle).
data Quad = Quad
  { qP0 :: {-# UNPACK #-} !V3, qP1 :: {-# UNPACK #-} !V3, qP2 :: {-# UNPACK #-} !V3, qP3 :: {-# UNPACK #-} !V3   -- ^ 10, 11, 12, 13
  , qElevation :: !(Maybe Double)                    -- ^ 38
  , qThickness :: !(Maybe Double)
  , qExtrusion :: !(Maybe V3)
  } deriving (Eq, Show, Generic, NFData)

data Face3d = Face3d
  { fP0 :: {-# UNPACK #-} !V3, fP1 :: {-# UNPACK #-} !V3, fP2 :: {-# UNPACK #-} !V3, fP3 :: {-# UNPACK #-} !V3
  , fInvisible :: !(Maybe Int)                       -- ^ 70, bit set: that edge is invisible
  } deriving (Eq, Show, Generic, NFData)

-- | RAY and XLINE.
data XLine = XLine
  { xlStart :: {-# UNPACK #-} !V3
  , xlDirection :: {-# UNPACK #-} !V3             -- ^ unit vector
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

ellipseCodec :: Codec Ellipse
ellipseCodec = codec (Ellipse v3zero (V3 1 0 0) 1 0 (2 * pi) Nothing)
  [ mark "AcDbEllipse"
  , pt3 "center" 10 (lens elCenter (\r v -> r { elCenter = v }))
  , pt3 "major_axis" 11 (lens elMajorAxis (\r v -> r { elMajorAxis = v }))
  , mpt3 "extrusion" 210 (lens elExtrusion (\r v -> r { elExtrusion = v }))
  , dbl "ratio" 40 (lens elRatio (\r v -> r { elRatio = v }))
  , dbl "start_param" 41 (lens elStartParam (\r v -> r { elStartParam = v }))
  , dbl "end_param" 42 (lens elEndParam (\r v -> r { elEndParam = v })) ]

quadCodec :: Codec Quad
quadCodec = codec (Quad v3zero v3zero v3zero v3zero Nothing Nothing Nothing)
  [ mark "AcDbTrace"
  , pt3 "vtx0" 10 (lens qP0 (\r v -> r { qP0 = v }))
  , pt3 "vtx1" 11 (lens qP1 (\r v -> r { qP1 = v }))
  , pt3 "vtx2" 12 (lens qP2 (\r v -> r { qP2 = v }))
  , pt3 "vtx3" 13 (lens qP3 (\r v -> r { qP3 = v }))
  , mdbl "elevation" 38 (lens qElevation (\r v -> r { qElevation = v }))
  , mdbl "thickness" 39 (lens qThickness (\r v -> r { qThickness = v }))
  , mpt3 "extrusion" 210 (lens qExtrusion (\r v -> r { qExtrusion = v })) ]

faceCodec :: Codec Face3d
faceCodec = codec (Face3d v3zero v3zero v3zero v3zero Nothing)
  [ mark "AcDbFace"
  , pt3 "vtx0" 10 (lens fP0 (\r v -> r { fP0 = v }))
  , pt3 "vtx1" 11 (lens fP1 (\r v -> r { fP1 = v }))
  , pt3 "vtx2" 12 (lens fP2 (\r v -> r { fP2 = v }))
  , pt3 "vtx3" 13 (lens fP3 (\r v -> r { fP3 = v }))
  , mint "invisible_edges" 70 (lens fInvisible (\r v -> r { fInvisible = v })) ]

xlineCodec :: Codec XLine
xlineCodec = codec (XLine v3zero (V3 0 0 1))
  [ mark "AcDbXline"
  , pt3 "start" 10 (lens xlStart (\r v -> r { xlStart = v }))
  , pt3 "unit_vector" 11 (lens xlDirection (\r v -> r { xlDirection = v })) ]

-- | A LINE written without the field table: 'lineCodec' says the same
-- (the test suite compares the two on every sample), only slower, and
-- LINEs are most of what a drawing holds.
lineOut :: Line -> Out
lineOut (Line a b th ext) f =
  tMarker "AcDbLine" f <> tV3 10 a f <> tV3 11 b f <> tMaybeD 39 th f
  <> (case ext of Nothing -> mempty; Just v -> tV3 210 v f)

geoCodecs :: [BodyCodec GeoBody]
geoCodecs =
  [ (entityCodec "LINE" True lineCodec GLine (\g -> case g of GLine r -> Just r; _ -> Nothing))
      { bcEncode = \g -> case g of GLine r -> lineOut r; _ -> mempty }
  , entityCodec "POINT" True pointCodec GPoint (\g -> case g of GPoint r -> Just r; _ -> Nothing)
  , entityCodec "CIRCLE" True circleCodec GCircle (\g -> case g of GCircle r -> Just r; _ -> Nothing)
  , entityCodec "ARC" True arcCodec GArc (\g -> case g of GArc r -> Just r; _ -> Nothing)
  , entityCodec "ELLIPSE" True ellipseCodec GEllipse (\g -> case g of GEllipse r -> Just r; _ -> Nothing)
  , entityCodec "SOLID" True quadCodec GSolid (\g -> case g of GSolid r -> Just r; _ -> Nothing)
  , entityCodec "TRACE" True quadCodec GTrace (\g -> case g of GTrace r -> Just r; _ -> Nothing)
  , entityCodec "3DFACE" True faceCodec G3dFace (\g -> case g of G3dFace r -> Just r; _ -> Nothing)
  , entityCodec "RAY" True xlineCodec GRay (\g -> case g of GRay r -> Just r; _ -> Nothing)
  , entityCodec "XLINE" True xlineCodec GXLine (\g -> case g of GXLine r -> Just r; _ -> Nothing)
  ]
