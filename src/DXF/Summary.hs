{-# LANGUAGE OverloadedStrings #-}
-- | What the library found in a file, as JSON, for comparison with what
-- ezdxf finds (@tools/compare.py@).
module DXF.Summary
  ( summary, summaryBytes
  ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import DXF.Classes
import DXF.Document
import DXF.Entity
import DXF.Header
import DXF.Json
import DXF.Tables
import DXF.Tag

summary :: Document -> J
summary d = JO
  [ ("version", JS (docVersion d))
  , ("header", JO [(hvName v, JA [JS x | Tag _ x <- hvTags v]) | v <- hdrVars (docHeader d)])
  , ("classes", JA (map classDump (docClasses d)))
  , ("tables", JO [(tabName t, JA (map dumpEntity (tabEntries t))) | t <- docTables d])
  , ("blocks", JA [JO [("name", JS (blockName b)), ("head", dumpEntity (blockHead b)), ("entities", JA (map dumpEntity (blockEntities b)))] | b <- docBlocks d])
  , ("entities", JA (map dumpEntity (docEntities d)))
  , ("objects", JA (map dumpEntity (docObjects d)))
  ]
  where
    blockName b = case [t | Tag 2 t <- rawName b] of (t : _) -> t; [] -> ""
    rawName b = case dumpEntity (blockHead b) of
      JO kv | Just (JO as) <- lookup "attribs" kv, Just (JS n) <- lookup "name" as -> [Tag 2 n]
      _ -> []

summaryBytes :: Document -> BL.ByteString
summaryBytes = BB.toLazyByteString . jsonOut . summary
