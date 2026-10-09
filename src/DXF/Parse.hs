{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | A file's bytes to a 'Document'.  The tags are consumed as they are
-- produced: each entity is decoded, forced, and its tags dropped, so the
-- file is never held as tags.
module DXF.Parse
  ( parseDXF, parseDocument, parseFile
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate, throw, try)
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafePerformIO)
import DXF.Classes
import DXF.Document
import DXF.Entity
import DXF.Header
import DXF.Tables
import DXF.Tag
import DXF.Tag.Scan

-- | Parse, reporting a structure error as 'Left'.
parseDXF :: B.ByteString -> Either DxfError Document
parseDXF bs = unsafePerformIO (try (evaluate (force (parseDocument bs))))
{-# NOINLINE parseDXF #-}

parseFile :: FilePath -> IO (Either DxfError Document)
parseFile p = B.readFile p >>= \bs -> try (evaluate (force (parseDocument bs)))

-- | Parse; throws 'DxfError' (when the result is forced) on a structure error.
parseDocument :: B.ByteString -> Document
parseDocument bs = Document fmt' pre secs
  where
    (fmt, ts) = scanTags bs
    (pre, rest) = break isSection ts
    secs = sections rest
    hdr = case [h | SHeader h <- secs] of (h : _) -> h; [] -> Header [] []
    fmt' = fmt { fmtR12 = headerIsR12 hdr }

isSection :: Tag -> Bool
isSection (Tag 0 "SECTION") = True
isSection _ = False

sections :: [Tag] -> [Section]
sections (Tag 0 "SECTION" : Tag 2 name : rest) =
  let (s, rest') = section (trim name) rest in s `seq` (s : sections rest')
sections (Tag 0 "EOF" : _) = []
sections [] = []
sections (Tag c v : _) = throw (DxfError 0 ("unexpected tag outside a section: " ++ show (c, v)))

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

section :: B.ByteString -> [Tag] -> (Section, [Tag])
section name ts = case name of
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
    entities t = let (es, rest) = chunkLoop (\h b -> decodeEntity (h : b)) t in (linkEntities es, rest)

splitEnd :: [Tag] -> ([Tag], [Tag])
splitEnd ts = let (body, rest) = break (\t -> t == Tag 0 "ENDSEC") ts in (body, drop 1 rest)

groupBlocks :: [Entity] -> [Block]
groupBlocks [] = []
groupBlocks (e : es)
  | eType e == "BLOCK" =
      let (body, rest) = break ((== "ENDBLK") . eType) es
      in Block e body (case rest of (x : _) -> Just x; [] -> Nothing) : groupBlocks (drop 1 rest)
  | otherwise = throw (DxfError 0 ("entity outside a block in BLOCKS: " ++ show (eType e)))
