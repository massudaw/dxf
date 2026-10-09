{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | VIEWPORT, IMAGE, WIPEOUT, underlays, ACIS entities, and other graphical entities.
module DXF.Entity.Misc
  ( MiscBody (..), miscCodecs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec

-- | Placeholder until the group is written.
data MiscBody = MiscBodyStub
  deriving (Eq, Show, Generic, NFData)

miscCodecs :: [BodyCodec MiscBody]
miscCodecs = []
