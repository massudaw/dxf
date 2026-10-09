{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | Polylines, splines, inserts: LWPOLYLINE, POLYLINE, VERTEX, SEQEND,
-- SPLINE, INSERT.  A POLYLINE's vertices and SEQEND, an INSERT's
-- attributes and SEQEND are the 'DXF.Entity.eKids' of the entity.
module DXF.Entity.Poly
  ( PolyBody (..), polyCodecs
  , LwPolyline (..), LwVertex (..), Polyline (..), Vertex (..), Insert (..), Spline (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Json
import DXF.Number
import DXF.Tag.Out
import DXF.Vec

data PolyBody
  = PLwPolyline !LwPolyline
  | PPolyline !Polyline
  | PVertex !Vertex
  | PSeqEnd
  | PInsert !Insert
  | PSpline !Spline
  deriving (Eq, Show, Generic, NFData)

data LwVertex = LwVertex
  { lvX :: !Double, lvY :: !Double
  , lvStartWidth :: !(Maybe Double)   -- ^ 40
  , lvEndWidth   :: !(Maybe Double)   -- ^ 41
  , lvBulge      :: !(Maybe Double)   -- ^ 42
  , lvId         :: !(Maybe Int)      -- ^ 91
  } deriving (Eq, Show, Generic, NFData)

data LwPolyline = LwPolyline
  { lwFlags      :: !Int                -- ^ 70: 1 closed, 128 linetype generation
  , lwConstWidth :: !(Maybe Double)     -- ^ 43
  , lwElevation  :: !(Maybe Double)     -- ^ 38
  , lwThickness  :: !(Maybe Double)     -- ^ 39
  , lwExtrusion  :: !(Maybe V3)         -- ^ 210
  , lwVertices   :: [LwVertex]          -- ^ 10, 20, 40, 41, 42, 91 (the count, 90, is the length)
  } deriving (Eq, Show, Generic, NFData)

-- | POLYLINE: 2D, 3D, polygon mesh or polyface mesh by its flags.
data Polyline = Polyline
  { plFollow     :: !Int                -- ^ 66, vertices follow (1)
  , plElevation  :: !V3                 -- ^ 10
  , plFlags      :: !Int                -- ^ 70: 1 closed, 8 3D polyline, 16 mesh, 32 mesh closed in n, 64 polyface
  , plStartWidth :: !(Maybe Double)     -- ^ 40
  , plEndWidth   :: !(Maybe Double)     -- ^ 41
  , plMCount     :: !(Maybe Int)        -- ^ 71
  , plNCount     :: !(Maybe Int)        -- ^ 72
  , plMDensity   :: !(Maybe Int)        -- ^ 73
  , plNDensity   :: !(Maybe Int)        -- ^ 74
  , plSmoothType :: !(Maybe Int)        -- ^ 75
  , plThickness  :: !(Maybe Double)     -- ^ 39
  , plExtrusion  :: !(Maybe V3)         -- ^ 210
  } deriving (Eq, Show, Generic, NFData)

data Vertex = Vertex
  { vxLocation   :: !V3                 -- ^ 10
  , vxStartWidth :: !(Maybe Double)     -- ^ 40
  , vxEndWidth   :: !(Maybe Double)     -- ^ 41
  , vxBulge      :: !(Maybe Double)     -- ^ 42
  , vxFlags      :: !Int                -- ^ 70
  , vxTangent    :: !(Maybe Double)     -- ^ 50
  , vxIndex1     :: !(Maybe Int)        -- ^ 71 (polyface faces)
  , vxIndex2     :: !(Maybe Int)        -- ^ 72
  , vxIndex3     :: !(Maybe Int)        -- ^ 73
  , vxIndex4     :: !(Maybe Int)        -- ^ 74
  , vxId         :: !(Maybe Int)        -- ^ 91
  } deriving (Eq, Show, Generic, NFData)

data Insert = Insert
  { inFollow   :: !(Maybe Int)          -- ^ 66, attributes follow
  , inName     :: !B.ByteString         -- ^ 2, block name
  , inInsert   :: !V3                   -- ^ 10
  , inXScale   :: !(Maybe Double)       -- ^ 41
  , inYScale   :: !(Maybe Double)       -- ^ 42
  , inZScale   :: !(Maybe Double)       -- ^ 43
  , inRotation :: !(Maybe Double)       -- ^ 50
  , inColumns  :: !(Maybe Int)          -- ^ 70
  , inRows     :: !(Maybe Int)          -- ^ 71
  , inColSpace :: !(Maybe Double)       -- ^ 44
  , inRowSpace :: !(Maybe Double)       -- ^ 45
  , inExtrusion :: !(Maybe V3)          -- ^ 210
  } deriving (Eq, Show, Generic, NFData)

data Spline = Spline
  { spExtrusion :: !(Maybe V3)          -- ^ 210
  , spFlags     :: !Int                 -- ^ 70
  , spDegree    :: !Int                 -- ^ 71
  , spKnotTol   :: !(Maybe Double)      -- ^ 42
  , spCtrlTol   :: !(Maybe Double)      -- ^ 43
  , spFitTol    :: !(Maybe Double)      -- ^ 44
  , spStartTan  :: !(Maybe V3)          -- ^ 12
  , spEndTan    :: !(Maybe V3)          -- ^ 13
  , spKnots     :: [Double]             -- ^ 40 (the count, 72, is the length)
  , spWeights   :: [Double]             -- ^ 41
  , spControl   :: [V3]                 -- ^ 10 (count 73)
  , spFit       :: [V3]                 -- ^ 11 (count 74)
  } deriving (Eq, Show, Generic, NFData)

-- ---------------------------------------------------------------- LWPOLYLINE

-- | The vertices: 10 starts one, 20 gives its y, the rest belong to the last.
vertexField :: F LwPolyline
vertexField = custom "points" [10, 20, 40, 41, 42, 91] rd
  (foldMap wr . lwVertices)
  (\r -> Just (JA [JA [JD (lvX v), JD (lvY v), JD (m (lvStartWidth v)), JD (m (lvEndWidth v)), JD (m (lvBulge v))] | v <- lwVertices r]))
  (\r -> r { lwVertices = reverse (lwVertices r) })
  where
    m = maybe 0 id
    wr v = tD 10 (lvX v) <> tD 20 (lvY v) <> tMaybeI 91 (lvId v) <> tMaybeD 40 (lvStartWidth v)
      <> tMaybeD 41 (lvEndWidth v) <> tMaybeD 42 (lvBulge v)
    rd c v r = readNum c v >>= \d -> case (c, lwVertices r) of
      (10, vs) -> Just r { lwVertices = LwVertex d 0 Nothing Nothing Nothing Nothing : vs }
      (20, x : vs) -> Just r { lwVertices = x { lvY = d } : vs }
      (40, x : vs) | lvStartWidth x == Nothing -> Just r { lwVertices = x { lvStartWidth = Just d } : vs }
      (41, x : vs) | lvEndWidth x == Nothing -> Just r { lwVertices = x { lvEndWidth = Just d } : vs }
      (42, x : vs) | lvBulge x == Nothing -> Just r { lwVertices = x { lvBulge = Just d } : vs }
      (91, x : vs) | lvId x == Nothing -> Just r { lwVertices = x { lvId = Just (truncate d) } : vs }
      _ -> Nothing
    readNum _ = readDouble

lwCodec :: Codec LwPolyline
lwCodec = codec (LwPolyline 0 Nothing Nothing Nothing Nothing [])
  [ mark "AcDbPolyline"
  , custom "count" [90] (\_ _ r -> Just r) (tI 90 . length . lwVertices) (Just . JI . length . lwVertices) id
  , int "flags" 70 (lens lwFlags (\r v -> r { lwFlags = v }))
  , mdbl "const_width" 43 (lens lwConstWidth (\r v -> r { lwConstWidth = v }))
  , mdbl "elevation" 38 (lens lwElevation (\r v -> r { lwElevation = v }))
  , mdbl "thickness" 39 (lens lwThickness (\r v -> r { lwThickness = v }))
  , vertexField
  , mpt3 "extrusion" 210 (lens lwExtrusion (\r v -> r { lwExtrusion = v })) ]

-- ------------------------------------------------------------------ POLYLINE

polylineCodec :: Codec Polyline
polylineCodec = codec (Polyline 1 v3zero 0 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  [ markR "AcDb2dPolyline", markR "AcDb3dPolyline", markR "AcDbPolygonMesh", markR "AcDbPolyFaceMesh"
  , custom "" [] (\_ _ _ -> Nothing) (\r -> tMarker (marker (plFlags r))) (const Nothing) id
  , int "" 66 (lens plFollow (\r v -> r { plFollow = v }))
  , pt3 "elevation" 10 (lens plElevation (\r v -> r { plElevation = v }))
  , int "flags" 70 (lens plFlags (\r v -> r { plFlags = v }))
  , mdbl "default_start_width" 40 (lens plStartWidth (\r v -> r { plStartWidth = v }))
  , mdbl "default_end_width" 41 (lens plEndWidth (\r v -> r { plEndWidth = v }))
  , mint "m_count" 71 (lens plMCount (\r v -> r { plMCount = v }))
  , mint "n_count" 72 (lens plNCount (\r v -> r { plNCount = v }))
  , mint "m_smooth_density" 73 (lens plMDensity (\r v -> r { plMDensity = v }))
  , mint "n_smooth_density" 74 (lens plNDensity (\r v -> r { plNDensity = v }))
  , mint "smooth_type" 75 (lens plSmoothType (\r v -> r { plSmoothType = v }))
  , mdbl "thickness" 39 (lens plThickness (\r v -> r { plThickness = v }))
  , mpt3 "extrusion" 210 (lens plExtrusion (\r v -> r { plExtrusion = v })) ]
  where
    marker f | f .&. 64 /= 0 = "AcDbPolyFaceMesh"
             | f .&. 16 /= 0 = "AcDbPolygonMesh"
             | f .&. 8 /= 0 = "AcDb3dPolyline"
             | otherwise = "AcDb2dPolyline"

vertexCodec :: Codec Vertex
vertexCodec = codec (Vertex v3zero Nothing Nothing Nothing 0 Nothing Nothing Nothing Nothing Nothing Nothing)
  [ markR "AcDbVertex", markR "AcDb2dVertex", markR "AcDb3dPolylineVertex", markR "AcDbPolygonMeshVertex"
  , markR "AcDbPolyFaceMeshVertex", markR "AcDbFaceRecord"
  , custom "" [] (\_ _ _ -> Nothing) (\r -> tMarker "AcDbVertex" <> tMarker (marker (vxFlags r))) (const Nothing) id
  , pt3 "location" 10 (lens vxLocation (\r v -> r { vxLocation = v }))
  , mdbl "start_width" 40 (lens vxStartWidth (\r v -> r { vxStartWidth = v }))
  , mdbl "end_width" 41 (lens vxEndWidth (\r v -> r { vxEndWidth = v }))
  , mdbl "bulge" 42 (lens vxBulge (\r v -> r { vxBulge = v }))
  , int "flags" 70 (lens vxFlags (\r v -> r { vxFlags = v }))
  , mdbl "tangent" 50 (lens vxTangent (\r v -> r { vxTangent = v }))
  , mint "vtx0" 71 (lens vxIndex1 (\r v -> r { vxIndex1 = v }))
  , mint "vtx1" 72 (lens vxIndex2 (\r v -> r { vxIndex2 = v }))
  , mint "vtx2" 73 (lens vxIndex3 (\r v -> r { vxIndex3 = v }))
  , mint "vtx3" 74 (lens vxIndex4 (\r v -> r { vxIndex4 = v }))
  , mint "vertex_identifier" 91 (lens vxId (\r v -> r { vxId = v })) ]
  where
    marker f | f .&. 192 == 192 = "AcDbPolyFaceMeshVertex"
             | f .&. 128 /= 0 = "AcDbFaceRecord"
             | f .&. 64 /= 0 = "AcDbPolygonMeshVertex"
             | f .&. 32 /= 0 = "AcDb3dPolylineVertex"
             | otherwise = "AcDb2dVertex"

-- -------------------------------------------------------------------- INSERT

insertCodec :: Codec Insert
insertCodec = codec (Insert Nothing "" v3zero Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  [ mark "AcDbBlockReference"
  , mint "attribs_follow" 66 (lens inFollow (\r v -> r { inFollow = v }))
  , str "name" 2 (lens inName (\r v -> r { inName = v }))
  , pt3 "insert" 10 (lens inInsert (\r v -> r { inInsert = v }))
  , mdbl "xscale" 41 (lens inXScale (\r v -> r { inXScale = v }))
  , mdbl "yscale" 42 (lens inYScale (\r v -> r { inYScale = v }))
  , mdbl "zscale" 43 (lens inZScale (\r v -> r { inZScale = v }))
  , mdbl "rotation" 50 (lens inRotation (\r v -> r { inRotation = v }))
  , mint "column_count" 70 (lens inColumns (\r v -> r { inColumns = v }))
  , mint "row_count" 71 (lens inRows (\r v -> r { inRows = v }))
  , mdbl "column_spacing" 44 (lens inColSpace (\r v -> r { inColSpace = v }))
  , mdbl "row_spacing" 45 (lens inRowSpace (\r v -> r { inRowSpace = v }))
  , mpt3 "extrusion" 210 (lens inExtrusion (\r v -> r { inExtrusion = v })) ]

-- -------------------------------------------------------------------- SPLINE

splineCodec :: Codec Spline
splineCodec = codec (Spline Nothing 0 3 Nothing Nothing Nothing Nothing Nothing [] [] [] [])
  [ mark "AcDbSpline"
  , mpt3 "extrusion" 210 (lens spExtrusion (\r v -> r { spExtrusion = v }))
  , int "flags" 70 (lens spFlags (\r v -> r { spFlags = v }))
  , int "degree" 71 (lens spDegree (\r v -> r { spDegree = v }))
  , count 72 (length . spKnots), count 73 (length . spControl), count 74 (length . spFit)
  , mdbl "knot_tolerance" 42 (lens spKnotTol (\r v -> r { spKnotTol = v }))
  , mdbl "control_point_tolerance" 43 (lens spCtrlTol (\r v -> r { spCtrlTol = v }))
  , mdbl "fit_tolerance" 44 (lens spFitTol (\r v -> r { spFitTol = v }))
  , mpt3 "start_tangent" 12 (lens spStartTan (\r v -> r { spStartTan = v }))
  , mpt3 "end_tangent" 13 (lens spEndTan (\r v -> r { spEndTan = v }))
  , dbls "knots" 40 (lens spKnots (\r v -> r { spKnots = v }))
  , dbls "weights" 41 (lens spWeights (\r v -> r { spWeights = v }))
  , pts3 "control_points" 10 (lens spControl (\r v -> r { spControl = v }))
  , pts3 "fit_points" 11 (lens spFit (\r v -> r { spFit = v })) ]
  where count c f = custom "" [c] (\_ _ r -> Just r) (tI c . f) (const Nothing) id

polyCodecs :: [BodyCodec PolyBody]
polyCodecs =
  [ entityCodec "LWPOLYLINE" True lwCodec PLwPolyline (\g -> case g of PLwPolyline r -> Just r; _ -> Nothing)
  , entityCodec "POLYLINE" True polylineCodec PPolyline (\g -> case g of PPolyline r -> Just r; _ -> Nothing)
  , entityCodec "VERTEX" True vertexCodec PVertex (\g -> case g of PVertex r -> Just r; _ -> Nothing)
  , entityCodec "SEQEND" True (codec () []) (const PSeqEnd) (\g -> case g of PSeqEnd -> Just (); _ -> Nothing)
  , entityCodec "INSERT" True insertCodec PInsert (\g -> case g of PInsert r -> Just r; _ -> Nothing)
  , entityCodec "SPLINE" True splineCodec PSpline (\g -> case g of PSpline r -> Just r; _ -> Nothing) ]
