{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | DIMENSION (all kinds), ARC_DIMENSION, LARGE_RADIAL_DIMENSION, LEADER, MLEADER.
module DXF.Entity.Dimension
  ( DimBody (..), dimCodecs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec

-- | Placeholder until the group is written.
data DimBody = DimBodyStub
  deriving (Eq, Show, Generic, NFData)

dimCodecs :: [BodyCodec DimBody]
dimCodecs = []
