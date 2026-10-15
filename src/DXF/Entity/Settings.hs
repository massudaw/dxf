{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | LAYOUT, PLOTSETTINGS and MLINESTYLE.  A plot-settings record is a bag
-- of variables ('Vars'); a layout is the plot settings subclass followed by
-- the layout subclass, whose codes mean something else.
module DXF.Entity.Settings
  ( SetBody (..), setCodecs
  , Layout (..), MLineStyle (..), plotSettingsVars, layoutVars
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Entity.Vars
import DXF.Json
import DXF.Number
import DXF.Tag
import DXF.Tag.Out

data SetBody
  = SLayout !Layout
  | SPlotSettings !Vars
  | SMLineStyle !MLineStyle
  deriving (Eq, Show, Generic, NFData)

data Layout = Layout
  { loPlot   :: [(Int, B.ByteString)]   -- ^ the AcDbPlotSettings subclass, as tags
  , loLayout :: [(Int, B.ByteString)]   -- ^ the AcDbLayout subclass, as tags
  , loInLayout :: !Bool                 -- ^ reading: past the AcDbLayout marker
  } deriving (Eq, Show, Generic, NFData)

data MLineStyle = MLineStyle
  { msName :: !B.ByteString             -- ^ 2
  , msFlags :: !(Maybe Int)             -- ^ 70
  , msDescription :: !(Maybe B.ByteString) -- ^ 3
  , msFillColor :: !(Maybe Int)         -- ^ 62 before the elements
  , msStartAngle :: !(Maybe Double)     -- ^ 51
  , msEndAngle :: !(Maybe Double)       -- ^ 52
  , msElements :: [(Double, Maybe Int, Maybe B.ByteString)] -- ^ offset (49), color (62), linetype (6); count 71 is the length
  } deriving (Eq, Show, Generic, NFData)

plotSettingsVars :: [VarDef]
plotSettingsVars =
  [ ("page_setup_name", 1, 1), ("plot_configuration_file", 2, 1), ("paper_size", 4, 1), ("plot_view_name", 6, 1)
  , ("left_margin", 40, 1), ("bottom_margin", 41, 1), ("right_margin", 42, 1), ("top_margin", 43, 1)
  , ("paper_width", 44, 1), ("paper_height", 45, 1), ("plot_origin_x_offset", 46, 1), ("plot_origin_y_offset", 47, 1)
  , ("plot_window_x1", 48, 1), ("plot_window_y1", 49, 1), ("plot_window_x2", 140, 1), ("plot_window_y2", 141, 1)
  , ("scale_numerator", 142, 1), ("scale_denominator", 143, 1), ("plot_layout_flags", 70, 1)
  , ("plot_paper_units", 72, 1), ("plot_rotation", 73, 1), ("plot_type", 74, 1), ("current_style_sheet", 7, 1)
  , ("standard_scale_type", 75, 1), ("shade_plot_mode", 76, 1), ("shade_plot_resolution_level", 77, 1)
  , ("shade_plot_custom_dpi", 78, 1), ("unit_factor", 147, 1), ("paper_image_origin_x", 148, 1)
  , ("paper_image_origin_y", 149, 1), ("shade_plot_handle", 333, 1) ]

layoutVars :: [VarDef]
layoutVars =
  [ ("name", 1, 1), ("layout_flags", 70, 1), ("taborder", 71, 1), ("limmin", 10, 2), ("limmax", 11, 2)
  , ("insert_base", 12, 3), ("extmin", 14, 3), ("extmax", 15, 3), ("elevation", 146, 1), ("ucs_origin", 13, 3)
  , ("ucs_xaxis", 16, 3), ("ucs_yaxis", 17, 3), ("ucs_type", 76, 1), ("block_record_handle", 330, 1)
  , ("viewport_handle", 331, 1), ("ucs_handle", 345, 1), ("base_ucs_handle", 346, 1) ]

defCodes :: [VarDef] -> [Int]
defCodes defs = [c + 10 * k | (_, c, n) <- defs, k <- [0 .. n - 1]]

layoutCodec :: Codec Layout
layoutCodec = codec (Layout [] [] False) $
  [ mark "AcDbPlotSettings", markRS "AcDbLayout" (\r -> r { loInLayout = True })
  , custom "" (defCodes plotSettingsVars ++ defCodes layoutVars) rd wr (const Nothing)
      (\r -> r { loPlot = reverse (loPlot r), loLayout = reverse (loLayout r) }) ]
  ++ dumpDefs plotSettingsVars loPlot ++ dumpDefs layoutVars loLayout
  where
    rd c v r | loInLayout r = Just r { loLayout = (c, v) : loLayout r }
             | otherwise = Just r { loPlot = (c, v) : loPlot r }
    wr r = foldMap raw (loPlot r) <> tMarker "AcDbLayout" <> foldMap raw (loLayout r)
    raw (c, v) = tRaw (Tag c v)

mlineStyleCodec :: Codec MLineStyle
mlineStyleCodec = codec (MLineStyle "" Nothing Nothing Nothing Nothing Nothing []) 
  [ mark "AcDbMlineStyle"
  , str "name" 2 (lens msName (\r v -> r { msName = v }))
  , mint "flags" 70 (lens msFlags (\r v -> r { msFlags = v }))
  , mstr "description" 3 (lens msDescription (\r v -> r { msDescription = v }))
  , custom "fill_color" [62] rd62 (maybe mempty (tI 62) . msFillColor) (fmap JI . msFillColor) id
  , mdbl "start_angle" 51 (lens msStartAngle (\r v -> r { msStartAngle = v }))
  , mdbl "end_angle" 52 (lens msEndAngle (\r v -> r { msEndAngle = v }))
  , custom "" [71] (\_ _ r -> Just r) (tI 71 . length . msElements) (const Nothing) id
  , custom "" [49, 6] rdEl (foldMap el . msElements) (const Nothing) (\r -> r { msElements = reverse (msElements r) }) ]
  where
    el (o, c, l) = tD 49 o <> tMaybeI 62 c <> tMaybeS 6 l
    rd62 _ v r = readInt v >>= \n -> case msElements r of
      [] | Nothing <- msFillColor r -> Just r { msFillColor = Just n }
      ((o, Nothing, l) : es) -> Just r { msElements = (o, Just n, l) : es }
      _ -> Nothing
    rdEl 49 v r = readDouble v >>= \d -> Just r { msElements = (d, Nothing, Nothing) : msElements r }
    rdEl _ v r = case msElements r of
      ((o, c, Nothing) : es) -> Just r { msElements = (o, c, Just v) : es }
      _ -> Nothing

setCodecs :: [BodyCodec SetBody]
setCodecs =
  [ entityCodec "LAYOUT" False layoutCodec SLayout (\g -> case g of SLayout r -> Just r; _ -> Nothing)
  , entityCodec "PLOTSETTINGS" False (bagCodec "AcDbPlotSettings" plotSettingsVars) SPlotSettings (\g -> case g of SPlotSettings r -> Just r; _ -> Nothing)
  , entityCodec "MLINESTYLE" False mlineStyleCodec SMLineStyle (\g -> case g of SMLineStyle r -> Just r; _ -> Nothing) ]
