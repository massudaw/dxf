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
import DXF.Header
import DXF.Tables
import DXF.Tag
import DXF.Tag.Scan
import DXF.Tag.Text (scanFrom)

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
    (fmt, ts) = scanTags bs
    (pre, rest) = break isSection ts
    secs = sections (if fmtBinary fmt || not fast then Nothing else Just bs) rest
    hdr = case [h | SHeader h <- secs] of (h : _) -> h; [] -> Header [] []
    fmt' = fmt { fmtR12 = headerIsR12 hdr }

isSection :: Tag -> Bool
isSection (Tag 0 "SECTION") = True
isSection _ = False

-- | The sections.  For a text file the bytes are given, so that entities
-- can be read from them directly ('fastEntity').
sections :: Maybe B.ByteString -> [Tag] -> [Section]
sections src (Tag 0 "SECTION" : Tag 2 name : rest) =
  let (s, rest') = section src (trim name) rest in s `seq` (s : sections src rest')
sections _ (Tag 0 "EOF" : _) = []
sections _ [] = []
sections _ (Tag c v : _) = throw (DxfError 0 ("unexpected tag outside a section: " <> v <> " (group code " <> B.pack (map (fromIntegral . fromEnum) (show c)) <> ")"))

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
-- from the bytes, the rest from their tags.  Each is forced and shares
-- layer and owner with the one before.
entityLoop :: Maybe B.ByteString -> [Tag] -> ([Entity], [Tag])
entityLoop src = go []
  where
    go !acc (Tag 0 "ENDSEC" : rest) = (reverse acc, rest)
    go !acc [] = (reverse acc, [])
    go !acc (t : rest)
      | Tag 0 ty <- t, Just bs <- src, Just (e, q) <- fastEntity bs prev ty = go (e : acc) (scanFrom bs q)
      | otherwise =
          let (body, more) = break isZero rest
              e0 = decodeEntity (t : body)
              !x = force e0 { eCommon = shareCommon (eCommon <$> prev) (eCommon e0) }
          in go (x : acc) more
      where prev = case acc of (e : _) -> Just e; [] -> Nothing

section :: Maybe B.ByteString -> B.ByteString -> [Tag] -> (Section, [Tag])
section src name ts = case name of
  "HEADER" -> let (body, rest) = splitEnd ts in (SHeader (force (headerFromTags body)), rest)
  "CLASSES" -> let (cs, rest) = chunkLoop (\_ b -> classFromTags b) ts in (SClasses cs, rest)
  "TABLES" ->
    let (chunks, rest) = chunkLoop (\t b -> t : b) ts
    in (STables (force (tablesFromChunks decodeEntity chunks)), rest)
  "BLOCKS" -> let (es, rest) = entities ts in (SBlocks (force (groupBlocks es)), rest)
  "ENTITIES" -> let (es, rest) = entities ts in (SEntities es, rest)
  "OBJECTS" -> let (es, rest) = entities ts in (SObjects es, rest)
  "THUMBNAILIMAGE" -> let (body, rest) = splitEnd ts in (SThumbnail (force body), rest)
  _ -> let (body, rest) = splitEnd ts in (SOther name (force body), rest)
  where
    entities t = let (es, rest) = entityLoop src t in (linkEntities es, rest)

splitEnd :: [Tag] -> ([Tag], [Tag])
splitEnd ts = let (body, rest) = break (\t -> t == Tag 0 "ENDSEC") ts in (body, drop 1 rest)

groupBlocks :: [Entity] -> [Block]
groupBlocks [] = []
groupBlocks (e : es)
  | eType e == "BLOCK" =
      let (body, rest) = break ((== "ENDBLK") . eType) es
      in Block e body (case rest of (x : _) -> Just x; [] -> Nothing) : groupBlocks (drop 1 rest)
  | otherwise = throw (DxfError 0 ("entity outside a block in BLOCKS: " <> eType e))
