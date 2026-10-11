{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the builder (src/DXF/Build.hs): drawings made from nothing are
-- written, parsed, and held to ezdxf (tools/build_check.py): its audit finds
-- nothing, it finds the same entities, layers and extended data, and a file
-- it reads and writes again parses to the same.
module Check.Build (checks) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate, isPrefixOf, sort)
import qualified Data.Text as T
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Check.House
import Check.Util
import DXF
import DXF.Build
import DXF.Entity.Block (BlkBody (..), BlockHead (..))
import DXF.Entity.Poly (PolyBody (..), LwPolyline (..), Insert (..))
import DXF.Entity.Tables (TabBody (..), Layer (..), AppId (..))
import DXF.Entity.Text (TxtBody (..), Text (..), TextData (..))

data Case = Case
  { cName :: String, cDrawing :: Drawing, cBytes :: B.ByteString
  , cParsed :: Either DxfError Document
  , cEzdxf :: [String], cRewritten :: Either String Document }

python :: FilePath
python = ".ref/venv/bin/python"

cases :: [(String, Drawing)]
cases = [ (show v, housePlan v) | v <- [R12, R2000, R2007, R2010, R2018] ] ++ [("many-R2010", manyLines R2010 2000), ("many-R12", manyLines R12 500)]

world :: [Case]
world = unsafePerformIO (mapM prepare cases)
{-# NOINLINE world #-}

prepare :: (String, Drawing) -> IO Case
prepare (n, d) = do
  tmp <- getTemporaryDirectory
  let f = tmp </> ("dxf-build-" ++ n ++ ".dxf"); g = tmp </> ("dxf-build-" ++ n ++ "-ezdxf.dxf")
  writeDrawing f d
  bytes <- B.readFile f
  (_, out, err) <- readProcessWithExitCode python ["-I", "tools/build_check.py", "describe", f] ""
  (code, _, err2) <- readProcessWithExitCode python ["-I", "tools/build_check.py", "rewrite", f, g] ""
  rew <- if code /= ExitSuccess then return (Left err2) else do
    ok <- doesFileExist g
    if not ok then return (Left "no file") else either (Left . show) Right . parseDXF <$> B.readFile g
  return (Case n d bytes (parseDXF bytes) (lines out ++ [err | not (null err)]) rew)

-- | What the python side prints, as this side prints it.
describe :: Document -> [String]
describe d = sort [l | l <- layerLines, not ("LAYER Defpoints" `isPrefixOf` l)] ++ sort appLines ++ map ent (docEntities d)
  ++ concat [("BLOCK " ++ name b) : map ent (blockEntities b) | b <- docBlocks d, userBlock (name b)]
  where
    enc = docEncoding d
    str = T.unpack . decodeText enc
    entries t = [e | tb <- docTables d, tabName tb == t, e <- tabEntries tb]
    layerLines = [ unwords ["LAYER", str (lyName l), show (abs (maybe 7 id (lyColor l))), str (maybe "Continuous" id (lyLinetype l))]
                 | e <- entries "LAYER", BTab (TLayer l) <- [eBody e] ]
    appLines = [ "APPID " ++ str (apName a) | e <- entries "APPID", BTab (TAppId a) <- [eBody e] ]
    name b = case eBody (blockHead b) of BBlk (BlockStart h) -> BC.unpack (bhName h); _ -> ""
    userBlock n = take 1 n `notElem` ["*", "$"]
    ent e = "ENT " ++ BC.unpack (eType e) ++ " " ++ str (maybe "0" id (cLayer (eCommon e))) ++ " " ++ xd e ++ extra e
    xd e = intercalate ";" [str (xdApp x) ++ "=" ++ intercalate "," (map val (xdValues x)) | x <- entityXData e]
    val v = case v of
      XStr s -> "1000:" ++ str s; XLayer s -> "1003:" ++ str s; XHandle s -> "1005:" ++ str s
      XReal r -> "1040:" ++ show r; XInt i -> "1070:" ++ show i; XLong i -> "1071:" ++ show i; _ -> "?"
    extra e = case eBody e of
      BTxt (TText t) -> " " ++ str (tdText (txData t))
      BPoly (PInsert i) -> " " ++ str (inName i)
      BPoly (PPolyline _) -> " v=" ++ show (length [() | k <- eKids e, eType k == "VERTEX"])
      BPoly (PLwPolyline lw) -> " v=" ++ show (length (lwVertices lw))
      _ -> ""

-- | The extended data of every entity, model space and blocks.
allXData :: Document -> [[XData]]
allXData d = map entityXData (docEntities d ++ concatMap blockEntities (docBlocks d))

ezdxfLines :: Case -> [String]
ezdxfLines c = [l | l <- cEzdxf c, not (any (`isPrefixOf` l) ["AUDIT", "VERSION"]), not ("LAYER Defpoints" `isPrefixOf` l)]

perCase :: (Case -> Result) -> IO Result
perCase f = return (concatMap (\c -> map ((cName c ++ ": ") ++) (f c)) world)

firstDiff :: [String] -> [String] -> [String]
firstDiff a b = take 3 [ "ours " ++ show x ++ " ezdxf " ++ show y | (x, y) <- take n (zip (a ++ repeat "<end>") (b ++ repeat "<end>")), x /= y ]
  where n = max (length a) (length b)

checks :: [(String, IO Result)]
checks =
  [ ("build: no problems in the drawings", perCase (\c -> drawingProblems (cDrawing c)))
  , ("build: written file parses and prints as it is", perCase $ \c -> case cParsed c of
      Left e -> ["does not parse: " ++ show e]
      Right d -> failIf (BL.toStrict (documentBytes d) /= cBytes c) "print . parse differs from the file")
  , ("build: extended data written is extended data read", perCase $ \c -> case cParsed c of
      Left e -> ["does not parse: " ++ show e]
      Right d -> failIf (allXData d /= allXData (toDocument (cDrawing c))) "the extended data read differs from what was written"
        ++ failIf (all null (allXData d)) "no extended data at all")
  , ("build: ezdxf audit finds nothing", perCase $ \c ->
      failIf ("AUDIT errors 0 fixes 0" `notElem` cEzdxf c) ("audit: " ++ unwords (take 4 [l | l <- cEzdxf c, "AUDIT" `isPrefixOf` l])))
  , ("build: ezdxf finds the same entities, layers and extended data", perCase $ \c -> case cParsed c of
      Left e -> ["does not parse: " ++ show e]
      Right d -> firstDiff (filter (not . isPrefixOf "LAYER Defpoints") (describe d)) (ezdxfLines c))
  , ("build: a file ezdxf wrote again parses to the same", perCase $ \c -> case (cParsed c, cRewritten c) of
      (Right d, Right r) -> firstDiff (describe d) (filter (`notElem` ["APPID EZDXF", "APPID HATCHBACKGROUNDCOLOR"]) (describe r)) ++ failIf (allXData d /= allXData r) "extended data changed"
      (_, Left e) -> ["ezdxf could not rewrite: " ++ take 200 e]
      (Left e, _) -> ["does not parse: " ++ show e]) ]
