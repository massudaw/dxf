{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Writing a new drawing from nothing.
--
-- > let d = addLayer "WALLS" 5 "Continuous"
-- >       . addEntity (line (V3 0 0 0) (V3 5 0 0) & onLayer "WALLS"
-- >                      & withXData (XData "MYAPP" [XStr "wall-17"]))
-- >       $ newDrawing R2010
-- > writeDrawing "plan.dxf" d
--
-- A 'Drawing' collects layers, linetypes, blocks and entities;
-- 'toDocument' makes the "DXF.Document" (numbering every handle, filling
-- the tables, computing @$EXTMIN@ and @$EXTMAX@) which "DXF.Print" writes.
module DXF.Build
  ( Version (..), Drawing, newDrawing, drawingVersion
  , setUnits, addLayer, addLayerWith, addLinetype, addTextStyle, addAppId, addBlock
  , addEntity, addEntities
  , toDocument, drawingBytes, writeDrawing, drawingProblems, drawingText
  , Entity, eType, V2 (..), V3 (..)
  , module DXF.Build.Entities, module DXF.XData
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.List (foldl', mapAccumL)
import DXF.Build.Entities
import DXF.Build.Skeleton
import DXF.Document
import DXF.Encoding
import DXF.Entity
import DXF.Entity.Block
import DXF.Entity.Hatch
import DXF.Entity.Poly
import DXF.Entity.Tables
import DXF.Entity.Text
import DXF.Header
import DXF.Print
import DXF.Tables
import DXF.Tag
import DXF.Vec
import DXF.XData

-- | A drawing being built.  Lists are kept last-added first.
data Drawing = Drawing
  { dwVersion :: !Version, dwUnits :: !Int
  , dwLayers :: [Layer], dwLinetypes :: [Linetype], dwStyles :: [TextStyle], dwApps :: [B.ByteString]
  , dwBlocks :: [(B.ByteString, V3, [Entity])], dwModel :: ![Entity] }  -- strict: no chain of updates

-- | An empty drawing: layer @0@, linetype @Continuous@, text style @Standard@, no entities.
newDrawing :: Version -> Drawing
newDrawing v = Drawing v 0 [] [] [] [] [] []

drawingVersion :: Drawing -> Version
drawingVersion = dwVersion

-- | @$INSUNITS@: 1 inches, 2 feet, 4 millimetres, 5 centimetres, 6 metres (0 unitless).  Not in R12.
setUnits :: Int -> Drawing -> Drawing
setUnits u d = d { dwUnits = u }

-- | A layer: name, colour index (negative: layer off), linetype name.
addLayer :: B.ByteString -> Int -> B.ByteString -> Drawing -> Drawing
addLayer n c lt = addLayerWith (Layer n (Just 0) (Just c) Nothing (Just lt) Nothing Nothing Nothing Nothing Nothing)

-- | A layer with every attribute (a layer of that name replaces the earlier one).
addLayerWith :: Layer -> Drawing -> Drawing
addLayerWith l d = d { dwLayers = l : filter ((/= lyName l) . lyName) (dwLayers d) }

-- | A linetype: name, description, pattern (positive a dash, negative a gap, zero a dot).
addLinetype :: B.ByteString -> B.ByteString -> [Double] -> Drawing -> Drawing
addLinetype n desc pat d = d { dwLinetypes = Linetype n (Just desc) (Just 0) (Just 65) (Just (sum (map abs pat))) els : dwLinetypes d }
  where els = [LtElement x (Just 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing | x <- pat]

-- | A text style: name and font file (@arial.ttf@).
addTextStyle :: B.ByteString -> B.ByteString -> Drawing -> Drawing
addTextStyle n font d = d { dwStyles = TextStyle n (Just 0) (Just 0) (Just 1) (Just 0) (Just 0) (Just 2.5) (Just font) (Just "") : dwStyles d }

-- | Register an application name for extended data (done by itself for
-- every application the entities use).
addAppId :: B.ByteString -> Drawing -> Drawing
addAppId a d = d { dwApps = a : dwApps d }

-- | A block: name, base point, entities.
addBlock :: B.ByteString -> V3 -> [Entity] -> Drawing -> Drawing
addBlock n base es d = d { dwBlocks = (n, base, concatMap (adapt (dwVersion d)) es) : dwBlocks d }

-- | An entity into model space.
addEntity :: Entity -> Drawing -> Drawing
addEntity e d = d { dwModel = foldl' (flip (:)) (dwModel d) (adapt (dwVersion d) e) }

addEntities :: [Entity] -> Drawing -> Drawing
addEntities es d = foldl' (flip addEntity) d es

-- | Text as bytes in the drawing's encoding (UTF-8 from R2007, else the code page).
drawingText :: Drawing -> T.Text -> B.ByteString
drawingText d = encodeText (encodingOf (versionCode (dwVersion d)) Nothing)

-- | What R12 has no entity for becomes what it has: an LWPOLYLINE a POLYLINE,
-- an MTEXT one TEXT per line, a HATCH the outlines of its polygon paths.
adapt :: Version -> Entity -> [Entity]
adapt v e
  | not (isR12 v) = [e]
  | otherwise = case eBody e of
      BPoly (PLwPolyline _) -> [lwToPolyline e]
      BTxt (TMText m) ->
        [ (text l (let V3 x y z = mtInsert m in V3 x (y - fromIntegral i * mtHeight m * 1.5) z) (mtHeight m)) { eCommon = eCommon e }
        | (i, l) <- zip [0 :: Int ..] (lines' (mtextContent m)) ]
      BHatch (BHatchE (Hatch (Just h) _)) ->
        [ (polyline2d True [(p, maybe 0 id b) | (p, b) <- vs]) { eCommon = eCommon e } | PolyPath _ _ _ vs _ <- hPaths h ]
      _ -> [e]
  where
    lines' s = let (a, b) = B.breakSubstring "\\P" s in a : if B.null b then [] else lines' (B.drop 2 b)

-- | The drawing as a document.
toDocument :: Drawing -> Document
toDocument dw = Document defaultFmt [] (concat
  [ [SHeader (headerFor v (dwUnits dw) ext seed)], [SClasses (classesFor v) | not r12]
  , [STables tables, SBlocks blocks, SEntities model'], [SObjects (sObjects std) | not r12] ])
  where
    v = dwVersion dw
    r12 = isR12 v
    std = standard ext
    model = reverse (dwModel dw)
    userBlocks = reverse (dwBlocks dw)
    boxes = foldl (\m (n, b, es) -> maybe m (\bx -> M.insert n bx m) (boxOf (map (sub b) (concatMap (pointsOf m) es)))) M.empty userBlocks
      where sub (V3 a b c) (V3 x y z) = V3 (x - a) (y - b) (z - c)
    ext = boxOf (concatMap (pointsOf boxes) model)
    -- names in use
    usedLayers = S.toList (S.fromList [l | e <- allEntities, Just l <- [cLayer (eCommon e)]] `S.difference` S.fromList ("0" : map lyName (dwLayers dw)))
    allEntities = model ++ [e | (_, _, es) <- userBlocks, e <- es]
    apps = dedupe ("ACAD" : reverse (dwApps dw) ++ [a | e <- allEntities, Tag 1001 a <- cXData (eCommon e)])
    layers = layer0 : reverse (dwLayers dw) ++ [Layer n (Just 0) (Just 7) Nothing (Just "Continuous") Nothing Nothing Nothing Nothing Nothing | n <- usedLayers]
    layer0 = Layer "0" (Just 0) (Just 7) Nothing (Just "Continuous") Nothing Nothing Nothing Nothing Nothing
    lw l = if r12 then l { lyLineweight = Nothing } else l { lyLineweight = Just (maybe (-3) id (lyLineweight l)) }
    -- handles
    (n1, lyE) = numbered "LAYER" 0x100 [tabEntry "LAYER" (TLayer (lw l)) | l <- layers]
    lyE' = case lyE of (z : r) -> fixed "34" z : r; [] -> []
    (n2, ltE) = numbered "LTYPE" n1 [tabEntry "LTYPE" (TLinetype l) | l <- reverse (dwLinetypes dw)]
    (n3, stE) = numbered "STYLE" n2 [tabEntry "STYLE" (TStyle s) | s <- reverse (dwStyles dw)]
    (n4, apE) = numbered "APPID" n3 [tabEntry "APPID" (TAppId (AppId a (Just 0))) | a <- drop 1 apps]
    (n5, brE) = numbered "BLOCK_RECORD" n4 [tabEntry "BLOCK_RECORD" (TBlockRecord (BlockRecord n Nothing (Just 0) (Just 1) (Just 0))) | (n, _, _) <- userBlocks]
    (n6, blocks') = mapAccumL block n5 (zip brE userBlocks)
    block n (br, (name, base, es)) =
      let rec = hnd br
          (n', hd) = assign r12 rec n (blockHead' name base)
          (n'', es') = mapAccumL (assign r12 rec) n' es
          (n3', en) = assign r12 rec n'' (Entity "ENDBLK" blankCommon { cLayer = Just "0" } (BBlk BlockEnd) [])
      in (n3', Block hd es' (Just en))
    -- the seed by a strict count, the entities numbered as they are printed: no
    -- chain of a million counters to force at once, nothing made before it is written
    seed = foldl' (\n e -> if r12 then n else n + 1 + length (eKids e)) n6 model
    model' = go n6 model
      where go !_ [] = []
            go !n (e : es) = snd (assign r12 hModelBR n e) : go (if r12 then n else n + 1 + length (eKids e)) es
    blockHead' name base = Entity "BLOCK" blankCommon { cLayer = Just "0" } (BBlk (BlockStart (BlockHead name 0 base (Just name) (Just "") Nothing))) []
    blocks = (if r12 then map (rename . strip') else id) (sBlocks std) ++ blocks'
    strip' b = b { blockHead = stripIds (blockHead b), blockEnd = fmap stripIds (blockEnd b) }
    rename b = b { blockHead = renameHead (blockHead b) }
    renameHead h = case eBody h of
      BBlk (BlockStart bh) -> h { eBody = BBlk (BlockStart bh { bhName = dollar (bhName bh), bhName2 = fmap dollar (bhName2 bh) }) }
      _ -> h
    dollar n = "$" <> B.drop 1 n
    -- tables
    ids = if r12 then stripIds else id
    tables =
      [ tableFor "VPORT" [ids (sVport std)], tableFor "LTYPE" (map ids (sLtypes std) ++ ltE), tableFor "LAYER" lyE'
      , tableFor "STYLE" (ids (sStyle std) : stE), tableFor "VIEW" [], tableFor "UCS" []
      , tableFor "APPID" (ids (sApp std) : apE), tableFor "DIMSTYLE" [ids (sDim std)] ]
      ++ [tableFor "BLOCK_RECORD" (sRecords std ++ brE) | not r12]
    tableFor name es = Table name (if r12 then blankCommon else blankCommon { cHandle = Just (tableHandle name), cOwner = Just "0" })
      Nothing (if name == "DIMSTYLE" && not r12 then Just (length es) else Nothing) es
    numbered tab n es = mapAccumL (assign' (tableHandle tab)) n es
    assign' owner n e = if r12 then (n, e) else let (n', e') = assign False owner n e in (n', e')
    fixed h e = if r12 then e else withIds h "22" e
    hnd e = maybe "0" id (cHandle (eCommon e))

-- | Give an entity (and its children) a handle and an owner, from a counter.
-- Vertices are owned like the polyline, its SEQEND by the polyline; all take its layer.
assign :: Bool -> B.ByteString -> Int -> Entity -> (Int, Entity)
assign r12 owner n e = (n', e { eCommon = c { cHandle = h }, eKids = ks })
  where
    c = (eCommon e) { cOwner = if r12 then Nothing else Just owner }
    hh = hex n
    h = if r12 then Nothing else Just hh
    layer = maybe "0" id (cLayer (eCommon e))
    (n', ks) = if r12 then (n, map kid0 (eKids e)) else mapAccumL kid (n + 1) (eKids e)
    kid0 k = k { eCommon = (eCommon k) { cLayer = Just layer } }
    kid m k = (m + 1, k { eCommon = (eCommon k) { cHandle = Just (hex m), cLayer = Just layer
                                               , cOwner = Just (if eType k == "SEQEND" then hh else owner) } })

dedupe :: [B.ByteString] -> [B.ByteString]
dedupe = go S.empty
  where go _ [] = []
        go s (x : xs) = if S.member x s then go s xs else x : go (S.insert x s) xs

type Box = (V3, V3)

boxOf :: [V3] -> Maybe Box
boxOf [] = Nothing
boxOf (p : ps) = Just (foldl (\(V3 a b c, V3 x y z) (V3 i j k) -> (V3 (min a i) (min b j) (min c k), V3 (max x i) (max y j) (max z k))) (p, p) ps)

-- | The points an entity covers; an INSERT by the corners of its block's box.
pointsOf :: M.Map B.ByteString Box -> Entity -> [V3]
pointsOf boxes e = case eBody e of
  BPoly (PInsert i) | Just (V3 x0 y0 z0, V3 x1 y1 z1) <- M.lookup (inName i) boxes ->
    let sx = maybe 1 id (inXScale i); sy = maybe 1 id (inYScale i); a = maybe 0 id (inRotation i) * pi / 180
        V3 ix iy iz = inInsert i
        corner x y z = let x' = x * sx; y' = y * sy in V3 (ix + x' * cos a - y' * sin a) (iy + x' * sin a + y' * cos a) (iz + z)
    in [corner x y z | x <- [x0, x1], y <- [y0, y1], z <- [z0, z1]]
  _ -> entityPoints e

-- | The bytes of the file.
drawingBytes :: Drawing -> BL.ByteString
drawingBytes = documentBytes . toDocument

writeDrawing :: FilePath -> Drawing -> IO ()
writeDrawing p = writeDocument p . toDocument

-- | What AutoCAD would refuse or ezdxf's audit would report: extended data
-- AutoCAD cannot read, an INSERT of a block that is not there, a linetype
-- or text style that is not defined.
drawingProblems :: Drawing -> [String]
drawingProblems dw =
  [ p | e <- es, x <- entityXData e, Just p <- [xdataProblem x] ]
  ++ [ "INSERT of undefined block " ++ show (inName i) | e <- es, BPoly (PInsert i) <- [eBody e], inName i `S.notMember` blockNames ]
  ++ [ "undefined linetype " ++ show l | l <- dedupeS [l | e <- es, Just l <- [cLinetype (eCommon e)], l `S.notMember` ltNames] ]
  where
    es = reverse (dwModel dw) ++ [e | (_, _, b) <- dwBlocks dw, e <- b]
    blockNames = S.fromList [n | (n, _, _) <- dwBlocks dw]
    ltNames = S.fromList (["ByLayer", "ByBlock", "Continuous"] ++ [ltName l | l <- dwLinetypes dw])
    dedupeS = S.toList . S.fromList
