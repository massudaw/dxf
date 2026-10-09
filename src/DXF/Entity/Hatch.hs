{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | HATCH and MPOLYGON.
module DXF.Entity.Hatch
  ( HatchBody (..), hatchCodecs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec

-- | Placeholder until the group is written.
data HatchBody = HatchBodyStub
  deriving (Eq, Show, Generic, NFData)

hatchCodecs :: [BodyCodec HatchBody]
hatchCodecs = []
