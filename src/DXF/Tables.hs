{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | The TABLES section: @0 TABLE@, a head, entries, @0 ENDTAB@.  Entries
-- are 'Entity's (LAYER, LTYPE, STYLE, DIMSTYLE, VPORT, VIEW, UCS, APPID,
-- BLOCK_RECORD).  The entry count in the head is not stored: it is the
-- number of entries, written as such.
module DXF.Tables
  ( Table (..), tablesFromChunks, tableOut
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Entity
import DXF.Tag
import DXF.Tag.Out

data Table = Table
  { tabName    :: !B.ByteString  -- ^ @LAYER@, @LTYPE@...
  , tabCommon  :: !Common        -- ^ handle, owner, extended data of the table itself
  , tabMax     :: !(Maybe Int)   -- ^ 70 as read; written as the larger of this and the number of entries
  , tabDimCount :: !(Maybe Int)  -- ^ 71 (DIMSTYLE only), same
  , tabEntries :: [Entity]
  } deriving (Eq, Show, Generic, NFData)

-- | The part of the head that is the table's own.
data Head = Head { hName :: B.ByteString, hMax :: Maybe Int, hDim :: Maybe Int }

headCodec :: Codec Head
headCodec = codec (Head "" Nothing Nothing)
  [ str "" 2 (lens hName (\r v -> r { hName = v }))
  , mark "AcDbSymbolTable"
  , mint "" 70 (lens hMax (\r v -> r { hMax = v }))
  , mark "AcDbDimStyleTable"
  , mint "" 71 (lens hDim (\r v -> r { hDim = v })) ]

-- | Tables from the chunks of the section (each chunk the tags of one
-- @0 X@ record).  Entries are decoded by the given function.
tablesFromChunks :: ([Tag] -> Entity) -> [[Tag]] -> [Table]
tablesFromChunks dec = go
  where
    go [] = []
    go ((Tag 0 "TABLE" : hd) : rest) =
      let (c, own) = splitTags False hd
          (h, left) = decodeWith headCodec own
          (es, rest') = break isEnd rest
          entries = map dec es
          n = length entries
          fix = fmap (max n)   -- a count smaller than the entries is repaired
          t = Table (hName h) c { cExtra = left } (fix (hMax h)) (fix (hDim h)) entries
      in t : go (drop 1 rest')
    go (_ : rest) = go rest
    isEnd ((Tag 0 "ENDTAB") : _) = True
    isEnd _ = False

tableOut :: Table -> Out
tableOut (Table n c mx dm es) =
  tS 0 "TABLE" <> tS 2 n <> tMaybeS 5 (cHandle c) <> foldMap tTags (cAppData c) <> tMaybeS 330 (cOwner c)
  <> tMarker "AcDbSymbolTable" <> tI 70 (cnt mx)
  <> (if n == "DIMSTYLE" then tMarker "AcDbDimStyleTable" else mempty)
  <> maybe mempty (\k -> tI 71 (max (length es) k)) dm
  <> tTags (cExtra c) <> tTags (cXData c)
  <> foldMap encodeEntity es <> tS 0 "ENDTAB"
  where cnt = max (length es) . maybe 0 id
