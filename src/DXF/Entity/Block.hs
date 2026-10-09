{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | BLOCK and ENDBLK: the two entities that bracket a block's entities.
module DXF.Entity.Block
  ( BlkBody (..), blkCodecs, BlockHead (..)
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Vec

data BlkBody
  = BlockStart !BlockHead
  | BlockEnd
  deriving (Eq, Show, Generic, NFData)

data BlockHead = BlockHead
  { bhName        :: !B.ByteString          -- ^ 2
  , bhFlags       :: !Int                   -- ^ 70: 1 anonymous, 2 has attribute definitions, 4 xref, 8 xref overlay...
  , bhBase        :: !V3                    -- ^ 10
  , bhName2       :: !(Maybe B.ByteString)  -- ^ 3, the name again
  , bhXrefPath    :: !(Maybe B.ByteString)  -- ^ 1
  , bhDescription :: !(Maybe B.ByteString)  -- ^ 4
  } deriving (Eq, Show, Generic, NFData)

headCodec :: Codec BlockHead
headCodec = codec (BlockHead "" 0 v3zero Nothing Nothing Nothing)
  [ mark "AcDbBlockBegin"
  , str "name" 2 (lens bhName (\r v -> r { bhName = v }))
  , int "flags" 70 (lens bhFlags (\r v -> r { bhFlags = v }))
  , pt3 "base_point" 10 (lens bhBase (\r v -> r { bhBase = v }))
  , mstr "name2" 3 (lens bhName2 (\r v -> r { bhName2 = v }))
  , mstr "xref_path" 1 (lens bhXrefPath (\r v -> r { bhXrefPath = v }))
  , mstr "description" 4 (lens bhDescription (\r v -> r { bhDescription = v })) ]

endCodec :: Codec ()
endCodec = codec () [mark "AcDbBlockEnd"]

blkCodecs :: [BodyCodec BlkBody]
blkCodecs =
  [ entityCodec "BLOCK" True headCodec BlockStart (\g -> case g of BlockStart r -> Just r; _ -> Nothing)
  , entityCodec "ENDBLK" True endCodec (const BlockEnd) (\g -> case g of BlockEnd -> Just (); _ -> Nothing) ]
