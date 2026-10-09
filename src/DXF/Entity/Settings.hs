{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | LAYOUT, PLOTSETTINGS, MLINESTYLE, VISUALSTYLE, MATERIAL and other settings objects.
module DXF.Entity.Settings
  ( SetBody (..), setCodecs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec

-- | Placeholder until the group is written.
data SetBody = SetBodyStub
  deriving (Eq, Show, Generic, NFData)

setCodecs :: [BodyCodec SetBody]
setCodecs = []
