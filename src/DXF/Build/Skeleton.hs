{-# LANGUAGE OverloadedStrings #-}
-- | What a drawing needs besides its own content, as AutoCAD and ezdxf
-- write it: the header variables, the classes, the standard table entries
-- (viewport, linetypes, text style, dimension style), the two layouts'
-- block records and blocks, and the objects that own them.  The handles of
-- these are fixed (below 0x100); the builder numbers everything else from
-- 0x100 on.
module DXF.Build.Skeleton
  ( Version (..), versionCode, isR12
  , Std (..), standard, headerFor, classesFor
  , hex, tableHandle, hModelBR, hPaperBR
  , tabEntry, withIds, stripIds
  ) where

import qualified Data.ByteString as B
import Data.List (foldl')
import DXF.Classes
import DXF.Document
import DXF.Entity
import DXF.Entity.Block
import DXF.Entity.Tables
import DXF.Header
import DXF.Tag
import DXF.Vec
import DXF.XData (intBytes, realBytes)

-- | The versions the builder writes.  R12 has no handles, no layouts and no
-- OBJECTS; the others are alike but for the code page and the number.
data Version = R12 | R2000 | R2004 | R2007 | R2010 | R2013 | R2018
  deriving (Eq, Ord, Show, Enum, Bounded)

versionCode :: Version -> B.ByteString
versionCode v = case v of
  R12 -> "AC1009"; R2000 -> "AC1015"; R2004 -> "AC1018"; R2007 -> "AC1021"
  R2010 -> "AC1024"; R2013 -> "AC1027"; R2018 -> "AC1032"

isR12 :: Version -> Bool
isR12 = (== R12)

-- | Handle text: upper case hexadecimal.
hex :: Int -> B.ByteString
hex n = if n < 16 then B.singleton (d n) else hex (n `div` 16) `B.snoc` d (n `mod` 16)
  where d k = if k < 10 then fromIntegral (48 + k) else fromIntegral (55 + k)

-- | The handle of a table by its name.
tableHandle :: B.ByteString -> B.ByteString
tableHandle n = case lookup n (zip ["VPORT", "LTYPE", "LAYER", "STYLE", "VIEW", "UCS", "APPID", "DIMSTYLE", "BLOCK_RECORD"] [0x20 :: Int ..]) of
  Just k -> hex k
  Nothing -> "0"

hModelBR, hPaperBR :: B.ByteString
hModelBR = "11"
hPaperBR = "12"

-- | A record from (code, value) pairs, the first the @0 TYPE@.
ent :: [(Int, B.ByteString)] -> Entity
ent = decodeEntity . map (uncurry Tag)

-- | A record kept as its tags (the objects: written as they stand).
raw :: [(Int, B.ByteString)] -> Entity
raw [] = Entity "" blankCommon (BRaw []) []
raw ((_, ty) : rest) = Entity ty blankCommon (BRaw (map (uncurry Tag) rest)) []

-- | Handle and owner of an entity.
withIds :: B.ByteString -> B.ByteString -> Entity -> Entity
withIds h o e = e { eCommon = (eCommon e) { cHandle = Just h, cOwner = Just o } }

-- | No handle, no owner (R12).
stripIds :: Entity -> Entity
stripIds e = e { eCommon = (eCommon e) { cHandle = Nothing, cOwner = Nothing }, eKids = map stripIds (eKids e) }

-- | A table entry owned by its table.
tabEntry :: B.ByteString -> TabBody -> Entity
tabEntry tab b = Entity (typ b) blankCommon { cOwner = Just (tableHandle tab) } (BTab b) []
  where typ x = case x of
          TLayer _ -> "LAYER"; TLinetype _ -> "LTYPE"; TStyle _ -> "STYLE"; TAppId _ -> "APPID"
          TBlockRecord _ -> "BLOCK_RECORD"; TUcs _ -> "UCS"; TView _ -> "VIEW"; TVPort _ -> "VPORT"; TDimStyle _ -> "DIMSTYLE"

-- | The standard entries and objects.
data Std = Std
  { sVport :: Entity, sLtypes :: [Entity], sStyle :: Entity, sApp :: Entity, sDim :: Entity
  , sRecords :: [Entity], sBlocks :: [Block], sObjects :: [Entity] }

-- | The standard content, for the extents of the drawing (to centre the
-- viewport on).
standard :: Maybe (V3, V3) -> Std
standard ext = Std
  { sVport = ent (vport ext)
  , sLtypes = [ withIds h "21" (tabEntry "LTYPE" (TLinetype (Linetype n (Just d) (Just 0) (Just 65) (Just 0) [])))
              | (h, n, d) <- [("31", "ByBlock", ""), ("32", "ByLayer", ""), ("33", "Continuous", "Solid line")] ]
  , sStyle = withIds "35" "23" (tabEntry "STYLE" (TStyle (TextStyle "Standard" (Just 0) (Just 0) (Just 1) (Just 0) (Just 0) (Just 2.5) (Just "txt") (Just ""))))
  , sApp = withIds "36" "26" (tabEntry "APPID" (TAppId (AppId "ACAD" (Just 0))))
  , sDim = ent dimstyle
  , sRecords = [ withIds hModelBR "28" (tabEntry "BLOCK_RECORD" (TBlockRecord (BlockRecord "*Model_Space" (Just "F") (Just 0) (Just 1) (Just 0))))
               , withIds hPaperBR "28" (tabEntry "BLOCK_RECORD" (TBlockRecord (BlockRecord "*Paper_Space" (Just "10") (Just 0) (Just 1) (Just 0)))) ]
  , sBlocks = [ spaceBlock "*Model_Space" "38" "39" hModelBR, spaceBlock "*Paper_Space" "3A" "3B" hPaperBR ]
  , sObjects = objects }

spaceBlock :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Block
spaceBlock name h hEnd owner = Block
  (withIds h owner (Entity "BLOCK" blankCommon { cLayer = Just "0" }
      (BBlk (BlockStart (BlockHead name 0 v3zero (Just name) (Just "") Nothing))) []))
  []
  (Just (withIds hEnd owner (Entity "ENDBLK" blankCommon { cLayer = Just "0" } (BBlk BlockEnd) [])))

vport :: Maybe (V3, V3) -> [(Int, B.ByteString)]
vport ext =
  [ (0, "VPORT"), (5, "30"), (330, "20"), (100, "AcDbSymbolTableRecord"), (100, "AcDbViewportTableRecord")
  , (2, "*Active"), (70, "0"), (10, "0.0"), (20, "0.0"), (11, "1.0"), (21, "1.0")
  , (12, r cx), (22, r cy), (13, "0.0"), (23, "0.0"), (14, "0.5"), (24, "0.5"), (15, "0.5"), (25, "0.5")
  , (16, "0.0"), (26, "0.0"), (36, "1.0"), (17, "0.0"), (27, "0.0"), (37, "0.0")
  , (40, r height), (41, "1.34"), (42, "50.0"), (43, "0.0"), (44, "0.0"), (50, "0.0"), (51, "0.0")
  , (71, "0"), (72, "1000"), (73, "1"), (74, "3"), (75, "0"), (76, "0"), (77, "0"), (78, "0")
  , (281, "0"), (65, "0"), (146, "0.0") ]
  where
    r = realBytes
    (cx, cy, height) = case ext of
      Just (V3 x0 y0 _, V3 x1 y1 _) -> ((x0 + x1) / 2, (y0 + y1) / 2, max 1 (max (y1 - y0) ((x1 - x0) / 1.34) * 1.1))
      Nothing -> (0, 0, 1000)

dimstyle :: [(Int, B.ByteString)]
dimstyle =
  [(0, "DIMSTYLE"), (105, "37"), (330, "27"), (100, "AcDbSymbolTableRecord"), (100, "AcDbDimStyleTableRecord"), (2, "Standard"), (70, "0")]
  ++ [(c, v) | (c, v) <-
      [ (40, "1.0"), (41, "2.5"), (42, "0.625"), (43, "3.75"), (44, "1.25"), (45, "0.0"), (46, "0.0"), (47, "0.0"), (48, "0.0")
      , (49, "2.5"), (140, "2.5"), (141, "2.5"), (142, "0.0"), (143, "0.03937007874"), (144, "1.0"), (145, "0.0")
      , (146, "1.0"), (147, "0.625"), (148, "0.0"), (69, "0"), (70, "0"), (71, "0"), (72, "0"), (73, "0"), (74, "0")
      , (75, "0"), (76, "0"), (77, "1"), (78, "8"), (79, "3"), (170, "0"), (171, "3"), (172, "1"), (173, "0"), (174, "0")
      , (175, "0"), (176, "0"), (177, "0"), (178, "0"), (179, "2"), (271, "2"), (272, "2"), (273, "2"), (274, "3")
      , (275, "0"), (276, "0"), (277, "2"), (278, "44"), (279, "0"), (280, "0"), (281, "0"), (282, "0"), (283, "0")
      , (284, "8"), (285, "0"), (286, "0"), (288, "0"), (289, "3"), (290, "0"), (371, "-2"), (372, "-2") ]]

-- | The objects of a drawing from R2000: the root dictionary and what it
-- owns, the two layouts.
objects :: [Entity]
objects =
  [ raw [(0, "DICTIONARY"), (5, "A"), (330, "0"), (100, "AcDbDictionary"), (281, "1"), (3, "ACAD_GROUP"), (350, "B")
        , (3, "ACAD_LAYOUT"), (350, "C"), (3, "ACAD_PLOTSTYLENAME"), (350, "D")]
  , raw [(0, "DICTIONARY"), (5, "B"), (330, "A"), (100, "AcDbDictionary"), (281, "1")]
  , raw [(0, "DICTIONARY"), (5, "C"), (330, "A"), (100, "AcDbDictionary"), (281, "1"), (3, "Model"), (350, "F"), (3, "Layout1"), (350, "10")]
  , raw [(0, "ACDBDICTIONARYWDFLT"), (5, "D"), (330, "A"), (100, "AcDbDictionary"), (281, "1"), (3, "Normal"), (350, "E")
        , (100, "AcDbDictionaryWithDefault"), (340, "E")]
  , raw [(0, "ACDBPLACEHOLDER"), (5, "E"), (330, "D")]
  , layout "F" "Model" "1024" "0" hModelBR
  , layout "10" "Layout1" "0" "1" hPaperBR ]

layout :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Entity
layout h name flags order owner = raw $
  [ (0, "LAYOUT"), (5, h), (330, "C"), (100, "AcDbPlotSettings"), (1, ""), (4, "A3"), (6, ""), (40, "7.5"), (41, "20.0")
  , (42, "7.5"), (43, "20.0"), (44, "420.0"), (45, "297.0"), (46, "0.0"), (47, "0.0"), (48, "0.0"), (49, "0.0")
  , (140, "0.0"), (141, "0.0"), (142, "1.0"), (143, "1.0"), (70, flags), (72, "1"), (73, "0"), (74, "5"), (7, "")
  , (75, "16"), (76, "0"), (77, "2"), (78, "300"), (147, "1.0"), (148, "0.0"), (149, "0.0")
  , (100, "AcDbLayout"), (1, name), (70, "1"), (71, order), (10, "0.0"), (20, "0.0"), (11, "420.0"), (21, "297.0")
  , (12, "0.0"), (22, "0.0"), (32, "0.0"), (14, "1e+20"), (24, "1e+20"), (34, "1e+20"), (15, "-1e+20"), (25, "-1e+20")
  , (35, "-1e+20"), (146, "0.0"), (13, "0.0"), (23, "0.0"), (33, "0.0"), (16, "1.0"), (26, "0.0"), (36, "0.0")
  , (17, "0.0"), (27, "1.0"), (37, "0.0"), (76, "1"), (330, owner) ]

-- | The classes the objects above need (R2000 and later).
classesFor :: Version -> [Class]
classesFor v
  | isR12 v = []
  | otherwise = [ classFromTags [Tag 1 n, Tag 2 c, Tag 3 "ObjectDBX Classes", Tag 90 "0", Tag 91 "0", Tag 280 "0", Tag 281 "0"]
                | (n, c) <- [("ACDBDICTIONARYWDFLT", "AcDbDictionaryWithDefault"), ("ACDBPLACEHOLDER", "AcDbPlaceHolder"), ("LAYOUT", "AcDbLayout")] ]

-- | The HEADER: the version, the extents, the units, and the variables a
-- reader expects; the handle seed is the first handle not used.
headerFor :: Version -> Int -> Maybe (V3, V3) -> Int -> Header
headerFor v units ext seed = Header [] (foldl' (flip add) [] vars)
  where
    add x xs = xs ++ [x]
    s n c x = HeaderVar n [Tag c x]
    i n x = s n 70 (intBytes x)
    d n c x = s n c (realBytes x)
    p3 n (V3 x y z) = HeaderVar n [Tag 10 (realBytes x), Tag 20 (realBytes y), Tag 30 (realBytes z)]
    p2 n x y = HeaderVar n [Tag 10 (realBytes x), Tag 20 (realBytes y)]
    (lo, hi) = maybe (V3 1e20 1e20 1e20, V3 (-1e20) (-1e20) (-1e20)) id ext
    new = not (isR12 v)
    vars =
      [s "$ACADVER" 1 (versionCode v), s "$DWGCODEPAGE" 3 "ANSI_1252"] ++ [s "$HANDSEED" 5 (hex seed) | new]
      ++ [p3 "$INSBASE" v3zero, p3 "$EXTMIN" lo, p3 "$EXTMAX" hi, p2 "$LIMMIN" 0 0, p2 "$LIMMAX" 420 297
         , i "$ORTHOMODE" 0, i "$REGENMODE" 1, i "$FILLMODE" 1, i "$QTEXTMODE" 0, i "$MIRRTEXT" 0
         , d "$LTSCALE" 40 1, i "$ATTMODE" 1, d "$TEXTSIZE" 40 2.5, d "$TRACEWID" 40 1
         , s "$TEXTSTYLE" 7 "Standard", s "$CLAYER" 8 "0", s "$CELTYPE" 6 "ByLayer", i "$CECOLOR" 256 ]
         ++ [d "$CELTSCALE" 40 1 | new]
         ++ [s "$DIMSTYLE" 2 "Standard", i "$LUNITS" 2, i "$LUPREC" 4, i "$AUNITS" 0, i "$AUPREC" 0
         , d "$ANGBASE" 50 0, i "$ANGDIR" 0, i "$PDMODE" 0, d "$PDSIZE" 40 0, d "$PLINEWID" 40 0
         , p3 "$UCSORG" v3zero, p3 "$UCSXDIR" (V3 1 0 0), p3 "$UCSYDIR" (V3 0 1 0), i "$WORLDVIEW" 1, i "$PSLTSCALE" 1 ]
         ++ [i "$TILEMODE" 1 | new]
         ++ [i "$INSUNITS" units | new] ++ [i "$MEASUREMENT" 1 | new]
