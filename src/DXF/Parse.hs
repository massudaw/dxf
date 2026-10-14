{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | A file's bytes to a 'Document'.  The tags are consumed as they are
-- produced: each entity is decoded, forced, and its tags dropped, so the
-- file is never held as tags.
module DXF.Parse
  ( parseDXF, parseGeneral, parseDocument, parseFile
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate, throw, try)
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafePerformIO)
import DXF.Classes
import DXF.Document
import DXF.Entity
import DXF.Entity.Fast
import DXF.Entity.Interned
import DXF.Intern
import DXF.Header
import DXF.Tables
import DXF.Tag
import DXF.Tag.Scan
import DXF.Tag.Text (T (..), nextTag, scanFrom, slice)

-- | Parse, reporting a structure error as 'Left'.
parseDXF :: B.ByteString -> Either DxfError Document
parseDXF bs = unsafePerformIO (try (evaluate (force (parseDocument bs))))
{-# NOINLINE parseDXF #-}

-- | Parse with the general decoder only (no entity read straight from
-- the bytes): what the fast decoders must agree with.
parseGeneral :: B.ByteString -> Either DxfError Document
parseGeneral bs = unsafePerformIO (try (evaluate (force (parseWith False bs))))
{-# NOINLINE parseGeneral #-}

parseFile :: FilePath -> IO (Either DxfError Document)
parseFile p = B.readFile p >>= \bs -> try (evaluate (force (parseDocument bs)))

-- | Parse; throws 'DxfError' (when the result is forced) on a structure error.
parseDocument :: B.ByteString -> Document
parseDocument = parseWith True

parseWith :: Bool -> B.ByteString -> Document
parseWith fast bs = Document fmt' pre secs
  where
    tbl = newTable bs
    (fmt, ts) = scanTags bs
    (pre, rest) = break isSection ts
    secs = sections tbl (if fmtBinary fmt || not fast then Nothing else Just bs) rest
    hdr = case [h | SHeader h <- secs] of (h : _) -> h; [] -> Header [] []
    fmt' = fmt { fmtR12 = headerIsR12 hdr }

-- | The table of strings for one parse (it depends on the input, so that
-- two parses never share one by accident of floating).
newTable :: B.ByteString -> Interner
newTable bs = withInterner (\t -> B.length bs `seq` t)
{-# NOINLINE newTable #-}

isSection :: Tag -> Bool
isSection (Tag 0 "SECTION") = True
isSection _ = False

-- | The sections.  For a text file the bytes are given, so that entities
-- can be read from them directly ('fastEntity').
sections :: Interner -> Maybe B.ByteString -> [Tag] -> [Section]
sections tbl src (Tag 0 "SECTION" : Tag 2 name : rest) =
  let (s, rest') = section tbl src (trim name) rest in s `seq` (s : sections tbl src rest')
sections _ _ (Tag 0 "EOF" : _) = []
sections _ _ [] = []
sections _ _ (Tag c v : _) = throw (DxfError 0 ("unexpected tag outside a section: " <> v <> " (group code " <> B.pack (map (fromIntegral . fromEnum) (show c)) <> ")"))

trim :: B.ByteString -> B.ByteString
trim = B.dropWhileEnd (== 32)

isZero :: Tag -> Bool
isZero (Tag 0 _) = True
isZero _ = False

-- | The chunks of a section (each from a @0 X@ tag to the next), decoded,
-- and what follows @0 ENDSEC@.
chunkLoop :: NFData a => (Tag -> [Tag] -> a) -> [Tag] -> ([a], [Tag])
chunkLoop f = go []
  where
    go !acc (Tag 0 "ENDSEC" : rest) = (reverse acc, rest)
    go !acc [] = (reverse acc, [])
    go !acc (t : rest) =
      let (body, more) = break isZero rest
          !x = force (f t body)
      in go (x : acc) more

-- | The entities of a section, up to its @ENDSEC@: the usual ones read
-- from the bytes, the rest from their tags.  Each is forced, its repeated
-- strings interned.
entityLoop :: Interner -> Maybe B.ByteString -> [Tag] -> ([Entity], [Tag])
entityLoop tbl src = go []
  where
    go !acc (Tag 0 "ENDSEC" : rest) = (reverse acc, rest)
    go !acc [] = (reverse acc, [])
    go !acc (t : rest)
      | Tag 0 ty <- t, Just bs <- src, Just nx <- afterType bs ty = fast bs acc ty nx
      | otherwise = general acc t rest
    -- from the bytes: while the entities are the usual ones, no tag is built
    fast bs !acc ty nx = case fastEntity tbl bs ty nx of
      Just (e, q) -> case nextTag bs q of
        T 0 s en nx'
          | ty' <- slice bs s en, ty' /= "ENDSEC", ty' /= "EOF" -> fast bs (e : acc) ty' nx'
        _ -> go (e : acc) (scanFrom bs q)
      Nothing -> general acc (Tag 0 ty) (scanFrom bs nx)
    general acc t rest =
      let (body, more) = break isZero rest
          !x = force (internEntity tbl (decodeEntity (t : body)))
      in go (x : acc) more

section :: Interner -> Maybe B.ByteString -> B.ByteString -> [Tag] -> (Section, [Tag])
section tbl src name ts = case name of
  "HEADER" -> let (body, rest) = splitEnd ts in (SHeader (force (headerFromTags body)), rest)
  "CLASSES" -> let (cs, rest) = chunkLoop (\_ b -> internClass tbl (classFromTags b)) ts in (SClasses cs, rest)
  "TABLES" ->
    let (chunks, rest) = chunkLoop (\t b -> t : b) ts
    in (STables (force (tablesFromChunks (internEntity tbl . decodeEntity) chunks)), rest)
  "BLOCKS" -> let (es, rest) = entities ts in (SBlocks (force (groupBlocks es)), rest)
  "ENTITIES" -> let (es, rest) = entities ts in (SEntities es, rest)
  "OBJECTS" -> let (es, rest) = entities ts in (SObjects es, rest)
  "THUMBNAILIMAGE" -> let (body, rest) = splitEnd ts in (SThumbnail (force body), rest)
  _ -> let (body, rest) = splitEnd ts in (SOther name (force body), rest)
  where
    entities t = let (es, rest) = entityLoop tbl src t in (linkEntities es, rest)

splitEnd :: [Tag] -> ([Tag], [Tag])
splitEnd ts = let (body, rest) = break (\t -> t == Tag 0 "ENDSEC") ts in (body, drop 1 rest)

groupBlocks :: [Entity] -> [Block]
groupBlocks [] = []
groupBlocks (e : es)
  | eType e == "BLOCK" =
      let (body, rest) = break ((== "ENDBLK") . eType) es
      in Block e body (case rest of (x : _) -> Just x; [] -> Nothing) : groupBlocks (drop 1 rest)
  | otherwise = throw (DxfError 0 ("entity outside a block in BLOCKS: " <> eType e))
