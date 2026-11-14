{-# LANGUAGE OverloadedStrings #-}
-- | A house plan made with the builder: walls as polylines on layers, door
-- blocks inserted, texts, a hatch, a roof of 3D faces; every entity carries
-- the id of its model element in extended data.
module Check.House (housePlan, manyLines, manyMade) where

import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import DXF.Build

-- | Number the entities: each gets its model id (@HOUSEID@: a string, the
-- number, a real), the walls a second application's data too.
tagged :: String -> [Entity] -> [Entity]
tagged prefix es =
  [ withXData (XData "HOUSEID" [XStr (BC.pack (prefix ++ show n)), XInt n, XReal (fromIntegral n / 4)])
      (if eType e == "LWPOLYLINE" then withXData (XData "REVIT" [XStr "Wall:Basic 200", XLong (100000 + n)]) e else e)
  | (n, e) <- zip [1 ..] es ]

rect :: Double -> Double -> Double -> Double -> [V2]
rect x0 y0 x1 y1 = [V2 x0 y0, V2 x1 y0, V2 x1 y1, V2 x0 y1]

-- | The plan, in the given version.
housePlan :: Version -> Drawing
housePlan v = foldl (flip ($)) (newDrawing v)
  [ setUnits 4
  , addLinetype "DASHED" "Dashed __ __ __" [12, -6]
  , addTextStyle "ARIAL" "arial.ttf"
  , addLayer "WALLS" 5 "Continuous", addLayer "DOORS" 3 "Continuous", addLayer "FURNITURE" 2 "DASHED"
  , addLayer "ANNOTATION" 7 "Continuous", addLayer "FLOOR" 8 "Continuous", addLayer "ROOF" 1 "Continuous"
  , addLayer "HIDDEN" (-9) "Continuous"
  , addAppId "HOUSEID"
  , addBlock "DOOR" (V3 0 0 0) (tagged "door-part-" [onLayer "DOORS" (line (V3 0 0 0) (V3 900 0 0)), onLayer "DOORS" (arc (V3 0 0 0) 900 0 90)])
  , addBlock "WINDOW" (V3 0 0 0) (tagged "window-part-" [onLayer "DOORS" (lwpolyline True (rect 0 (-100) 1200 100))])
  , addEntities (tagged "wall-" walls)
  , addEntities (tagged "door-" doors)
  , addEntities (tagged "item-" furniture)
  , addEntities (tagged "text-" texts)
  , addEntities (tagged "floor-" floors)
  , addEntities (tagged "roof-" roof) ]
  where
    walls = map (onLayer "WALLS")
      [ lwpolyline True (rect 0 0 10000 8000)
      , lwpolyline False [V2 5000 0, V2 5000 5000], lwpolyline False [V2 0 5000, V2 5000 5000]
      , lwpolyline False [V2 5000 3000, V2 10000 3000]
      , polyline2d False [(V2 6000 6000, 0), (V2 7000 6000, 0.5), (V2 7000 7000, 0)] ]
    doors = map (onLayer "DOORS")
      [ insert "DOOR" (V3 2000 0 0), rotated 90 (insert "DOOR" (V3 5000 2500 0))
      , scaled 1.2 1.2 1 (insert "DOOR" (V3 8000 3000 0)), insert "WINDOW" (V3 7000 8000 0) ]
    furniture = map (onLayer "FURNITURE")
      [ circle (V3 2500 2500 0) 600, arc (V3 7500 5500 0) 800 30 210
      , withLinetype "DASHED" (lwpolyline True (rect 6000 400 9000 1400)), point (V3 1000 4000 0)
      , withColor 1 (withLineweight 50 (lwpolylineBulge False [(V2 500 6000, 0), (V2 1500 6000, 0.4142), (V2 1500 7000, 0)])) ]
    texts = map (onLayer "ANNOTATION")
      [ alignedText 1 2 (text (drawingText (newDrawing v) "Küche") (V3 2500 4000 0) 250)
      , textStyle "ARIAL" (rotated 0 (text "Bedroom" (V3 7500 2000 0) 250))
      , mtext "Living room\\P25 m2" (V3 6000 7500 0) 200 ]
    floors = map (onLayer "FLOOR")
      [ hatchSolid [rect 5100 3100 9900 7900], hatchPattern "ANSI31" 45 100 (ansi31 100) [rect 100 100 4900 4900, rect 1000 1000 2000 2000] ]
    roof = map (onLayer "ROOF")
      [ face3d (V3 0 0 3000) (V3 10000 0 3000) (V3 10000 4000 5000) (V3 0 4000 5000)
      , face3d (V3 0 8000 3000) (V3 10000 8000 3000) (V3 10000 4000 5000) (V3 10000 4000 5000)
      , polyline3d False [V3 0 4000 5000, V3 5000 4000 5200, V3 10000 4000 5000] ]

-- | n lines, each with extended data: the writing benchmark.
manyLines :: Version -> Int -> Drawing
manyLines v n = foldl (flip ($)) (newDrawing v) (addLayer "LINES" 3 "Continuous" : [ addEntity (manyLine i) | i <- [1 .. n] ])

-- | The same drawing, its lines between the first ten and the last ten made, not held
-- ('addGenerated'): the same bytes.
manyMade :: Version -> Int -> Drawing
manyMade v n = foldl (flip ($)) (newDrawing v) (addLayer "LINES" 3 "Continuous" : [ addEntity (manyLine i) | i <- [1 .. 10] ]
  ++ [addGenerated (n - 20) (manyLine . (+ 11))] ++ [ addEntity (manyLine i) | i <- [n - 9 .. n] ])

manyLine :: Int -> Entity
manyLine i = e i
  where
    e i = withXData (XData "HOUSEID" [XStr (BC.pack ("line-" ++ show i)), XInt (i `mod` 32000), XReal (fromIntegral i / 8)])
            (onLayer "LINES" (line (V3 (fromIntegral i) 0 0) (V3 (fromIntegral i * 1.5) (fromIntegral (i `mod` 97) * 10.25) 0)))

_unused :: Text
_unused = mempty
