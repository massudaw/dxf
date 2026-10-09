{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Table entries: LAYER, LTYPE, STYLE, DIMSTYLE, VPORT, VIEW, UCS, APPID, BLOCK_RECORD.
module DXF.Entity.Tables
  ( TabBody (..), tabCodecs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec

-- | Placeholder until the group is written.
data TabBody = TabBodyStub
  deriving (Eq, Show, Generic, NFData)

tabCodecs :: [BodyCodec TabBody]
tabCodecs = []
