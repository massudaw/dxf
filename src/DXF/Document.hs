{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | A DXF file as a value: its layout, and its sections in file order.
module DXF.Document
  ( Document (..), Section (..), Block (..)
  , docHeader, docClasses, docTables, docBlocks, docEntities, docObjects, docThumbnail
  , docVersion, docEncoding, sectionName
  ) where

import Control.DeepSeq (NFData (..))
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Classes
import DXF.Encoding
import DXF.Entity
import DXF.Header
import DXF.Tables
import DXF.Tag

-- | A BLOCK, its entities and its ENDBLK.
data Block = Block
  { blockHead     :: !Entity          -- ^ the BLOCK entity (name, flags, base point)
  , blockEntities :: [Entity]
  , blockEnd      :: !(Maybe Entity)  -- ^ the ENDBLK entity
  } deriving (Eq, Show, Generic, NFData)

data Section
  = SHeader !Header
  | SClasses [Class]
  | STables [Table]
  | SBlocks [Block]
  | SEntities [Entity]
  | SObjects [Entity]
  | SThumbnail [Tag]               -- ^ THUMBNAILIMAGE: @90@ size, @310@ hex data
  | SOther !B.ByteString [Tag]     -- ^ a section the library has no structure for (ACDSDATA...), by name, as tags
  deriving (Eq, Show, Generic, NFData)

data Document = Document
  { docFmt      :: !Fmt        -- ^ the layout it was read in; 'fmtR12' follows the header when written
  , docPreamble :: [Tag]       -- ^ comments before the first section
  , docSections :: [Section]
  } deriving (Eq, Show, Generic)

instance NFData Fmt where rnf f = f `seq` ()
instance NFData Document

sectionName :: Section -> B.ByteString
sectionName s = case s of
  SHeader _ -> "HEADER"
  SClasses _ -> "CLASSES"
  STables _ -> "TABLES"
  SBlocks _ -> "BLOCKS"
  SEntities _ -> "ENTITIES"
  SObjects _ -> "OBJECTS"
  SThumbnail _ -> "THUMBNAILIMAGE"
  SOther n _ -> n

docHeader :: Document -> Header
docHeader d = case [h | SHeader h <- docSections d] of (h : _) -> h; [] -> Header [] []

docClasses :: Document -> [Class]
docClasses d = concat [c | SClasses c <- docSections d]

docTables :: Document -> [Table]
docTables d = concat [t | STables t <- docSections d]

docBlocks :: Document -> [Block]
docBlocks d = concat [b | SBlocks b <- docSections d]

docEntities :: Document -> [Entity]
docEntities d = concat [e | SEntities e <- docSections d]

docObjects :: Document -> [Entity]
docObjects d = concat [e | SObjects e <- docSections d]

docThumbnail :: Document -> [Tag]
docThumbnail d = concat [t | SThumbnail t <- docSections d]

docVersion :: Document -> B.ByteString
docVersion = headerVersion . docHeader

docEncoding :: Document -> Encoding
docEncoding d = let h = docHeader d in encodingOf (headerVersion h) (headerCodepage h)
