{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | VIEWPORT, IMAGE, WIPEOUT, underlays (PDF, DWF, DGN) and the ACIS
-- solids (3DSOLID, BODY, REGION).  The ACIS data is kept as the tags it
-- is; a viewport is a bag of variables ('Vars').
module DXF.Entity.Misc
  ( MiscBody (..), miscCodecs
  , Image (..), Underlay (..), Acis (..), viewportVars
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Entity.Vars
import DXF.Json
import DXF.Tag
import DXF.Tag.Out
import DXF.Vec

data MiscBody
  = MViewport !Vars
  | MImage !Image
  | MWipeout !Image
  | MUnderlay !Underlay
  | MAcis !Acis
  deriving (Eq, Show, Generic, NFData)

-- | IMAGE and WIPEOUT.
data Image = Image
  { imVersion  :: !(Maybe Int)          -- ^ 90, class version
  , imInsert   :: !V3                   -- ^ 10
  , imUPixel   :: !V3                   -- ^ 11, one pixel along u
  , imVPixel   :: !V3                   -- ^ 12
  , imSize     :: !V2                   -- ^ 13, in pixels
  , imDefHandle :: !(Maybe B.ByteString) -- ^ 340
  , imFlags    :: !(Maybe Int)          -- ^ 70
  , imClipping :: !(Maybe Int)          -- ^ 280
  , imBrightness :: !(Maybe Int)        -- ^ 281
  , imContrast :: !(Maybe Int)          -- ^ 282
  , imFade     :: !(Maybe Int)          -- ^ 283
  , imReactor  :: !(Maybe B.ByteString) -- ^ 360
  , imBoundaryType :: !(Maybe Int)      -- ^ 71
  , imClipMode :: !(Maybe Int)          -- ^ 290
  , imBoundary :: [V2]                  -- ^ 14 (the count, 91, is the length)
  } deriving (Eq, Show, Generic, NFData)

data Underlay = Underlay
  { ulDefHandle :: !(Maybe B.ByteString) -- ^ 340
  , ulInsert :: !V3                      -- ^ 10
  , ulScaleX, ulScaleY, ulScaleZ :: !(Maybe Double) -- ^ 41, 42, 43
  , ulRotation :: !(Maybe Double)        -- ^ 50
  , ulExtrusion :: !(Maybe V3)           -- ^ 210
  , ulFlags :: !(Maybe Int)              -- ^ 280
  , ulContrast :: !(Maybe Int)           -- ^ 281
  , ulFade :: !(Maybe Int)               -- ^ 282
  , ulBoundary :: [V2]                   -- ^ 11
  } deriving (Eq, Show, Generic, NFData)

-- | 3DSOLID, BODY, REGION: the modeler geometry subclass.
data Acis = Acis
  { acVersion :: !(Maybe Int)            -- ^ 70
  , acFlags :: !(Maybe Int)              -- ^ 290
  , acUid :: !(Maybe B.ByteString)       -- ^ 2
  , acHistory :: !(Maybe B.ByteString)   -- ^ 350 (3DSOLID)
  , acData :: [Tag]                      -- ^ the SAT lines (1, 3) or SAB chunks (310), in order
  } deriving (Eq, Show, Generic, NFData)

imageCodec :: B.ByteString -> Codec Image
imageCodec marker = codec (Image Nothing v3zero v3zero v3zero v2zero Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [])
  [ mark marker
  , mint "class_version" 90 (lens imVersion (\r v -> r { imVersion = v }))
  , pt3 "insert" 10 (lens imInsert (\r v -> r { imInsert = v }))
  , pt3 "u_pixel" 11 (lens imUPixel (\r v -> r { imUPixel = v }))
  , pt3 "v_pixel" 12 (lens imVPixel (\r v -> r { imVPixel = v }))
  , pt2 "image_size" 13 (lens imSize (\r v -> r { imSize = v }))
  , mstr "image_def_handle" 340 (lens imDefHandle (\r v -> r { imDefHandle = v }))
  , mint "flags" 70 (lens imFlags (\r v -> r { imFlags = v }))
  , mint "clipping" 280 (lens imClipping (\r v -> r { imClipping = v }))
  , mint "brightness" 281 (lens imBrightness (\r v -> r { imBrightness = v }))
  , mint "contrast" 282 (lens imContrast (\r v -> r { imContrast = v }))
  , mint "fade" 283 (lens imFade (\r v -> r { imFade = v }))
  , mstr "image_def_reactor_handle" 360 (lens imReactor (\r v -> r { imReactor = v }))
  , mint "clipping_boundary_type" 71 (lens imBoundaryType (\r v -> r { imBoundaryType = v }))
  , custom "count_boundary_points" [91] (\_ _ r -> Just r) (tI 91 . length . imBoundary) (Just . JI . length . imBoundary) id
  , pts2 "" 14 (lens imBoundary (\r v -> r { imBoundary = v }))
  , mint "clip_mode" 290 (lens imClipMode (\r v -> r { imClipMode = v })) ]

underlayCodec :: Codec Underlay
underlayCodec = codec (Underlay Nothing v3zero Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [])
  [ mark "AcDbUnderlayReference"
  , mstr "underlay_def_handle" 340 (lens ulDefHandle (\r v -> r { ulDefHandle = v }))
  , pt3 "insert" 10 (lens ulInsert (\r v -> r { ulInsert = v }))
  , mdbl "scale_x" 41 (lens ulScaleX (\r v -> r { ulScaleX = v }))
  , mdbl "scale_y" 42 (lens ulScaleY (\r v -> r { ulScaleY = v }))
  , mdbl "scale_z" 43 (lens ulScaleZ (\r v -> r { ulScaleZ = v }))
  , mdbl "rotation" 50 (lens ulRotation (\r v -> r { ulRotation = v }))
  , mpt3 "extrusion" 210 (lens ulExtrusion (\r v -> r { ulExtrusion = v }))
  , mint "flags" 280 (lens ulFlags (\r v -> r { ulFlags = v }))
  , mint "contrast" 281 (lens ulContrast (\r v -> r { ulContrast = v }))
  , mint "fade" 282 (lens ulFade (\r v -> r { ulFade = v }))
  , pts2 "" 11 (lens ulBoundary (\r v -> r { ulBoundary = v })) ]

acisCodec :: Codec Acis
acisCodec = codec (Acis Nothing Nothing Nothing Nothing [])
  [ mark "AcDbModelerGeometry", mark "AcDb3dSolid"
  , mint "version" 70 (lens acVersion (\r v -> r { acVersion = v }))
  , mint "flags" 290 (lens acFlags (\r v -> r { acFlags = v }))
  , mstr "uid" 2 (lens acUid (\r v -> r { acUid = v }))
  , custom "" ([1, 3] ++ [310 .. 319]) (\c v r -> Just r { acData = Tag c v : acData r }) (tTags . acData) (const Nothing)
      (\r -> r { acData = reverse (acData r) })
  , mstr "history_handle" 350 (lens acHistory (\r v -> r { acHistory = v })) ]

viewportVars :: [VarDef]
viewportVars =
  [ ("center", 10, 3), ("width", 40, 1), ("height", 41, 1), ("status", 68, 1), ("id", 69, 1), ("view_center_point", 12, 2)
  , ("snap_base_point", 13, 2), ("snap_spacing", 14, 2), ("grid_spacing", 15, 2), ("view_direction_vector", 16, 3)
  , ("view_target_point", 17, 3), ("perspective_lens_length", 42, 1), ("front_clip_plane_z_value", 43, 1)
  , ("back_clip_plane_z_value", 44, 1), ("view_height", 45, 1), ("snap_angle", 50, 1), ("view_twist_angle", 51, 1)
  , ("circle_zoom", 72, 1), ("flags", 90, 1), ("clipping_boundary_handle", 340, 1), ("plot_style_name", 1, 1)
  , ("render_mode", 281, 1), ("ucs_per_viewport", 71, 1), ("ucs_icon", 74, 1), ("ucs_origin", 110, 3)
  , ("ucs_x_axis", 111, 3), ("ucs_y_axis", 112, 3), ("ucs_handle", 345, 1), ("base_ucs_handle", 346, 1)
  , ("ucs_ortho_type", 79, 1), ("elevation", 146, 1), ("shade_plot_mode", 170, 1), ("grid_frequency", 61, 1)
  , ("background_handle", 332, 1), ("shade_plot_handle", 333, 1), ("visual_style_handle", 348, 1)
  , ("default_lighting_flag", 292, 1), ("default_lighting_type", 282, 1), ("view_brightness", 141, 1)
  , ("view_contrast", 142, 1), ("ambient_light_color_1", 63, 1), ("ambient_light_color_2", 421, 1)
  , ("ambient_light_color_3", 431, 1), ("sun_handle", 361, 1), ("ref_vp_object_1", 335, 1)
  , ("ref_vp_object_2", 343, 1), ("ref_vp_object_3", 344, 1), ("ref_vp_object_4", 91, 1) ]

miscCodecs :: [BodyCodec MiscBody]
miscCodecs =
  [ entityCodec "VIEWPORT" True (bagCodec "AcDbViewport" viewportVars) MViewport (\g -> case g of MViewport r -> Just r; _ -> Nothing)
  , entityCodec "IMAGE" True (imageCodec "AcDbRasterImage") MImage (\g -> case g of MImage r -> Just r; _ -> Nothing)
  , entityCodec "WIPEOUT" True (imageCodec "AcDbWipeout") MWipeout (\g -> case g of MWipeout r -> Just r; _ -> Nothing) ]
  ++ [ entityCodec t True underlayCodec MUnderlay (\g -> case g of MUnderlay r -> Just r; _ -> Nothing)
     | t <- ["PDFUNDERLAY", "DWFUNDERLAY", "DGNUNDERLAY"] ]
  ++ [ entityCodec t True acisCodec MAcis (\g -> case g of MAcis r -> Just r; _ -> Nothing)
     | t <- ["3DSOLID", "BODY", "REGION"] ]
