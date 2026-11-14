{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | Table entries: LAYER, LTYPE, STYLE, DIMSTYLE, VPORT, VIEW, UCS, APPID,
-- BLOCK_RECORD.  DIMSTYLE, VPORT and VIEW are 'Vars' (see "DXF.Entity.Vars").
module DXF.Entity.Tables
  ( TabBody (..), tabCodecs
  , Layer (..), Linetype (..), LtElement (..), TextStyle (..), Ucs (..), AppId (..), BlockRecord (..)
  , dimStyleVars, vportVars, viewVars
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Entity.Vars
import DXF.Json
import DXF.Number
import DXF.Tag.Out
import DXF.Vec

data TabBody
  = TLayer !Layer
  | TLinetype !Linetype
  | TStyle !TextStyle
  | TDimStyle !Vars
  | TVPort !Vars
  | TView !Vars
  | TUcs !Ucs
  | TAppId !AppId
  | TBlockRecord !BlockRecord
  deriving (Eq, Show, Generic, NFData)

data Layer = Layer
  { lyName :: !B.ByteString, lyFlags :: !(Maybe Int)   -- ^ 2, 70 (1 frozen, 2 frozen in new viewports, 4 locked)
  , lyColor :: !(Maybe Int)            -- ^ 62, negative: layer is off
  , lyTrueColor :: !(Maybe Int)        -- ^ 420
  , lyLinetype :: !(Maybe B.ByteString) -- ^ 6
  , lyPlot :: !(Maybe Int)             -- ^ 290
  , lyLineweight :: !(Maybe Int)       -- ^ 370
  , lyPlotStyle :: !(Maybe B.ByteString) -- ^ 390
  , lyMaterial :: !(Maybe B.ByteString)  -- ^ 347
  , lyUnknown :: !(Maybe B.ByteString)   -- ^ 348
  } deriving (Eq, Show, Generic, NFData)

-- | One element of a linetype pattern.
data LtElement = LtElement
  { leLength :: !Double                -- ^ 49
  , leFlags :: !(Maybe Int)            -- ^ 74
  , leShape :: !(Maybe Int)            -- ^ 75
  , leStyle :: !(Maybe B.ByteString)   -- ^ 340
  , leScale :: !(Maybe Double)         -- ^ 46
  , leRotation :: !(Maybe Double)      -- ^ 50
  , leXOffset :: !(Maybe Double)       -- ^ 44
  , leYOffset :: !(Maybe Double)       -- ^ 45
  , leText :: !(Maybe B.ByteString)    -- ^ 9
  } deriving (Eq, Show, Generic, NFData)

data Linetype = Linetype
  { ltName :: !B.ByteString, ltDescription :: !(Maybe B.ByteString) -- ^ 2, 3
  , ltFlags :: !(Maybe Int)            -- ^ 70
  , ltAlignment :: !(Maybe Int)        -- ^ 72
  , ltLength :: !(Maybe Double)        -- ^ 40, total pattern length
  , ltElements :: [LtElement]          -- ^ (the count, 73, is the length)
  } deriving (Eq, Show, Generic, NFData)

data TextStyle = TextStyle
  { tsName :: !B.ByteString, tsFlags :: !(Maybe Int)
  , tsHeight :: !(Maybe Double)        -- ^ 40
  , tsWidth :: !(Maybe Double)         -- ^ 41
  , tsOblique :: !(Maybe Double)       -- ^ 50
  , tsGenFlags :: !(Maybe Int)         -- ^ 71
  , tsLastHeight :: !(Maybe Double)    -- ^ 42
  , tsFont :: !(Maybe B.ByteString)    -- ^ 3
  , tsBigFont :: !(Maybe B.ByteString) -- ^ 4
  } deriving (Eq, Show, Generic, NFData)

data Ucs = Ucs
  { ucName :: !B.ByteString, ucFlags :: !(Maybe Int)
  , ucOrigin :: !V3, ucXAxis :: !V3, ucYAxis :: !V3   -- ^ 10, 11, 12
  } deriving (Eq, Show, Generic, NFData)

data AppId = AppId { apName :: !B.ByteString, apFlags :: !(Maybe Int) }
  deriving (Eq, Show, Generic, NFData)

data BlockRecord = BlockRecord
  { brName :: !B.ByteString
  , brLayout :: !(Maybe B.ByteString)  -- ^ 340, handle of the layout
  , brUnits :: !(Maybe Int)            -- ^ 70
  , brExplode :: !(Maybe Int)          -- ^ 280
  , brScale :: !(Maybe Int)            -- ^ 281
  } deriving (Eq, Show, Generic, NFData)

nameF :: (r -> B.ByteString) -> (r -> B.ByteString -> r) -> F r
nameF g s = str "name" 2 (lens g s)

layerCodec :: Codec Layer
layerCodec = codec (Layer "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  [ mark "AcDbSymbolTableRecord", mark "AcDbLayerTableRecord"
  , nameF lyName (\r v -> r { lyName = v })
  , mint "flags" 70 (lens lyFlags (\r v -> r { lyFlags = v }))
  , mint "color" 62 (lens lyColor (\r v -> r { lyColor = v }))
  , mstr "linetype" 6 (lens lyLinetype (\r v -> r { lyLinetype = v }))
  , mint "plot" 290 (lens lyPlot (\r v -> r { lyPlot = v }))
  , mint "lineweight" 370 (lens lyLineweight (\r v -> r { lyLineweight = v }))
  , mstr "plotstyle_handle" 390 (lens lyPlotStyle (\r v -> r { lyPlotStyle = v }))
  , mstr "material_handle" 347 (lens lyMaterial (\r v -> r { lyMaterial = v }))
  , mstr "unknown1" 348 (lens lyUnknown (\r v -> r { lyUnknown = v }))
  , mint "true_color" 420 (lens lyTrueColor (\r v -> r { lyTrueColor = v })) ]

-- | The pattern: 49 starts an element, the codes after it belong to it.
patternField :: F Linetype
patternField = custom "" [49, 74, 75, 340, 46, 50, 44, 45, 9] rd (foldMap el . ltElements) (const Nothing)
  (\r -> r { ltElements = reverse (ltElements r) })
  where
    el e = tD 49 (leLength e) <> tMaybeI 74 (leFlags e) <> tMaybeI 75 (leShape e) <> tMaybeS 340 (leStyle e)
      <> tMaybeD 46 (leScale e) <> tMaybeD 50 (leRotation e) <> tMaybeD 44 (leXOffset e)
      <> tMaybeD 45 (leYOffset e) <> tMaybeS 9 (leText e)
    rd 49 v r = readDouble v >>= \d -> Just r { ltElements = LtElement d Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing : ltElements r }
    rd c v r = case ltElements r of
      [] -> Nothing
      (e : es) -> upd c v e >>= \e' -> Just r { ltElements = e' : es }
    upd 74 v e | leFlags e == Nothing = (\n -> e { leFlags = Just n }) <$> readInt v
    upd 75 v e | leShape e == Nothing = (\n -> e { leShape = Just n }) <$> readInt v
    upd 340 v e | leStyle e == Nothing = Just e { leStyle = Just v }
    upd 46 v e | leScale e == Nothing = (\d -> e { leScale = Just d }) <$> readDouble v
    upd 50 v e | leRotation e == Nothing = (\d -> e { leRotation = Just d }) <$> readDouble v
    upd 44 v e | leXOffset e == Nothing = (\d -> e { leXOffset = Just d }) <$> readDouble v
    upd 45 v e | leYOffset e == Nothing = (\d -> e { leYOffset = Just d }) <$> readDouble v
    upd 9 v e | leText e == Nothing = Just e { leText = Just v }
    upd _ _ _ = Nothing

linetypeCodec :: Codec Linetype
linetypeCodec = codec (Linetype "" Nothing Nothing Nothing Nothing [])
  [ mark "AcDbSymbolTableRecord", mark "AcDbLinetypeTableRecord"
  , nameF ltName (\r v -> r { ltName = v })
  , mstr "description" 3 (lens ltDescription (\r v -> r { ltDescription = v }))
  , mint "flags" 70 (lens ltFlags (\r v -> r { ltFlags = v }))
  , mint "" 72 (lens ltAlignment (\r v -> r { ltAlignment = v }))
  , custom "" [73] (\_ _ r -> Just r) (tI 73 . length . ltElements) (const Nothing) id
  , mdbl "" 40 (lens ltLength (\r v -> r { ltLength = v }))
  , patternField ]

styleCodec :: Codec TextStyle
styleCodec = codec (TextStyle "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  [ mark "AcDbSymbolTableRecord", mark "AcDbTextStyleTableRecord"
  , nameF tsName (\r v -> r { tsName = v })
  , mint "flags" 70 (lens tsFlags (\r v -> r { tsFlags = v }))
  , mdbl "height" 40 (lens tsHeight (\r v -> r { tsHeight = v }))
  , mdbl "width" 41 (lens tsWidth (\r v -> r { tsWidth = v }))
  , mdbl "oblique" 50 (lens tsOblique (\r v -> r { tsOblique = v }))
  , mint "generation_flags" 71 (lens tsGenFlags (\r v -> r { tsGenFlags = v }))
  , mdbl "last_height" 42 (lens tsLastHeight (\r v -> r { tsLastHeight = v }))
  , mstr "font" 3 (lens tsFont (\r v -> r { tsFont = v }))
  , mstr "bigfont" 4 (lens tsBigFont (\r v -> r { tsBigFont = v })) ]

ucsCodec :: Codec Ucs
ucsCodec = codec (Ucs "" Nothing v3zero v3zero v3zero)
  [ mark "AcDbSymbolTableRecord", mark "AcDbUCSTableRecord"
  , nameF ucName (\r v -> r { ucName = v })
  , mint "flags" 70 (lens ucFlags (\r v -> r { ucFlags = v }))
  , pt3 "origin" 10 (lens ucOrigin (\r v -> r { ucOrigin = v }))
  , pt3 "xaxis" 11 (lens ucXAxis (\r v -> r { ucXAxis = v }))
  , pt3 "yaxis" 12 (lens ucYAxis (\r v -> r { ucYAxis = v })) ]

appidCodec :: Codec AppId
appidCodec = codec (AppId "" Nothing)
  [ mark "AcDbSymbolTableRecord", mark "AcDbRegAppTableRecord"
  , nameF apName (\r v -> r { apName = v })
  , mint "flags" 70 (lens apFlags (\r v -> r { apFlags = v })) ]

blockRecordCodec :: Codec BlockRecord
blockRecordCodec = codec (BlockRecord "" Nothing Nothing Nothing Nothing)
  [ mark "AcDbSymbolTableRecord", mark "AcDbBlockTableRecord"
  , nameF brName (\r v -> r { brName = v })
  , mstr "layout" 340 (lens brLayout (\r v -> r { brLayout = v }))
  , mint "units" 70 (lens brUnits (\r v -> r { brUnits = v }))
  , mint "explode" 280 (lens brExplode (\r v -> r { brExplode = v }))
  , mint "scale" 281 (lens brScale (\r v -> r { brScale = v })) ]

-- | DIMSTYLE variables: ezdxf name, group code, coordinates.
dimStyleVars :: [VarDef]
dimStyleVars =
  [ ("handle", 105, 1), ("dimpost", 3, 1), ("dimapost", 4, 1), ("dimblk", 5, 1), ("dimblk1", 6, 1), ("dimblk2", 7, 1)
  , ("dimscale", 40, 1), ("dimasz", 41, 1), ("dimexo", 42, 1), ("dimdli", 43, 1), ("dimexe", 44, 1), ("dimrnd", 45, 1)
  , ("dimdle", 46, 1), ("dimtp", 47, 1), ("dimtm", 48, 1), ("dimfxl", 49, 1), ("dimjogang", 50, 1), ("dimtfill", 69, 1)
  , ("dimtfillclr", 70, 1), ("dimtol", 71, 1), ("dimlim", 72, 1), ("dimtih", 73, 1), ("dimtoh", 74, 1), ("dimse1", 75, 1)
  , ("dimse2", 76, 1), ("dimtad", 77, 1), ("dimzin", 78, 1), ("dimazin", 79, 1), ("dimarcsym", 90, 1), ("dimtxt", 140, 1)
  , ("dimcen", 141, 1), ("dimtsz", 142, 1), ("dimaltf", 143, 1), ("dimlfac", 144, 1), ("dimtvp", 145, 1), ("dimtfac", 146, 1)
  , ("dimgap", 147, 1), ("dimaltrnd", 148, 1), ("dimalt", 170, 1), ("dimaltd", 171, 1), ("dimtofl", 172, 1), ("dimsah", 173, 1)
  , ("dimtix", 174, 1), ("dimsoxd", 175, 1), ("dimclrd", 176, 1), ("dimclre", 177, 1), ("dimclrt", 178, 1), ("dimadec", 179, 1)
  , ("dimunit", 270, 1), ("dimdec", 271, 1), ("dimtdec", 272, 1), ("dimaltu", 273, 1), ("dimalttd", 274, 1), ("dimaunit", 275, 1)
  , ("dimfrac", 276, 1), ("dimlunit", 277, 1), ("dimdsep", 278, 1), ("dimtmove", 279, 1), ("dimjust", 280, 1), ("dimsd1", 281, 1)
  , ("dimsd2", 282, 1), ("dimtolj", 283, 1), ("dimtzin", 284, 1), ("dimaltz", 285, 1), ("dimalttz", 286, 1), ("dimfit", 287, 1)
  , ("dimupt", 288, 1), ("dimatfit", 289, 1), ("dimfxlon", 290, 1), ("dimtxsty_handle", 340, 1), ("dimldrblk_handle", 341, 1)
  , ("dimblk_handle", 342, 1), ("dimblk1_handle", 343, 1), ("dimblk2_handle", 344, 1), ("dimltype_handle", 345, 1)
  , ("dimltex1_handle", 346, 1), ("dimltex2_handle", 347, 1), ("dimlwd", 371, 1), ("dimlwe", 372, 1) ]

vportVars :: [VarDef]
vportVars =
  [ ("lower_left", 10, 2), ("upper_right", 11, 2), ("center", 12, 2), ("snap_base", 13, 2), ("snap_spacing", 14, 2)
  , ("grid_spacing", 15, 2), ("direction", 16, 3), ("target", 17, 3), ("height", 40, 1), ("aspect_ratio", 41, 1)
  , ("focal_length", 42, 1), ("front_clipping", 43, 1), ("back_clipping", 44, 1), ("snap_rotation", 50, 1)
  , ("view_twist", 51, 1), ("view_mode", 71, 1), ("circle_sides", 72, 1), ("fast_zoom", 73, 1), ("ucs_icon", 74, 1)
  , ("snap_on", 75, 1), ("grid_on", 76, 1), ("snap_style", 77, 1), ("snap_isopair", 78, 1), ("plot_style_sheet", 1, 1)
  , ("render_mode", 281, 1), ("ucs_vp", 65, 1), ("ucs_origin", 110, 3), ("ucs_xaxis", 111, 3), ("ucs_yaxis", 112, 3)
  , ("ucs_handle", 345, 1), ("base_ucs_handle", 346, 1), ("ucs_ortho_type", 79, 1), ("elevation", 146, 1)
  , ("unknown1", 60, 1), ("shade_plot_setting", 170, 1), ("major_grid_lines", 61, 1), ("background_handle", 332, 1)
  , ("shade_plot_handle", 333, 1), ("visual_style_handle", 348, 1), ("default_lighting_on", 292, 1)
  , ("default_lighting_type", 282, 1), ("brightness", 141, 1), ("contrast", 142, 1), ("ambient_color_aci", 63, 1)
  , ("ambient_true_color", 421, 1), ("ambient_color_name", 431, 1), ("sun_handle", 361, 1) ]

viewVars :: [VarDef]
viewVars =
  [ ("height", 40, 1), ("width", 41, 1), ("center", 10, 2), ("direction", 11, 3), ("target", 12, 3)
  , ("focal_length", 42, 1), ("front_clipping", 43, 1), ("back_clipping", 44, 1), ("view_twist", 50, 1)
  , ("view_mode", 71, 1), ("render_mode", 281, 1), ("ucs", 72, 1), ("ucs_origin", 110, 3), ("ucs_xaxis", 111, 3)
  , ("ucs_yaxis", 112, 3), ("ucs_ortho_type", 79, 1), ("elevation", 146, 1), ("ucs_handle", 345, 1)
  , ("base_ucs_handle", 346, 1), ("camera_plottable", 73, 1), ("background_handle", 332, 1)
  , ("live_selection_handle", 334, 1), ("visual_style_handle", 348, 1), ("sun_handle", 361, 1) ]

tabCodecs :: [BodyCodec TabBody]
tabCodecs =
  [ entityCodec "LAYER" False layerCodec TLayer (\g -> case g of TLayer r -> Just r; _ -> Nothing)
  , entityCodec "LTYPE" False linetypeCodec TLinetype (\g -> case g of TLinetype r -> Just r; _ -> Nothing)
  , entityCodec "STYLE" False styleCodec TStyle (\g -> case g of TStyle r -> Just r; _ -> Nothing)
  , entityCodec "DIMSTYLE" False (varsCodec "AcDbDimStyleTableRecord" dimStyleVars) TDimStyle (\g -> case g of TDimStyle r -> Just r; _ -> Nothing)
  , entityCodec "VPORT" False (varsCodec "AcDbViewportTableRecord" vportVars) TVPort (\g -> case g of TVPort r -> Just r; _ -> Nothing)
  , entityCodec "VIEW" False (varsCodec "AcDbViewTableRecord" viewVars) TView (\g -> case g of TView r -> Just r; _ -> Nothing)
  , entityCodec "UCS" False ucsCodec TUcs (\g -> case g of TUcs r -> Just r; _ -> Nothing)
  , entityCodec "APPID" False appidCodec TAppId (\g -> case g of TAppId r -> Just r; _ -> Nothing)
  , entityCodec "BLOCK_RECORD" False blockRecordCodec TBlockRecord (\g -> case g of TBlockRecord r -> Just r; _ -> Nothing) ]

_unusedJ :: J
_unusedJ = JNull
