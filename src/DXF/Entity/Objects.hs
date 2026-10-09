{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Objects of the OBJECTS section: DICTIONARY, XRECORD, GROUP, IMAGEDEF and the like.
module DXF.Entity.Objects
  ( ObjBody (..), objCodecs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec

-- | Placeholder until the group is written.
data ObjBody = ObjBodyStub
  deriving (Eq, Show, Generic, NFData)

objCodecs :: [BodyCodec ObjBody]
objCodecs = []
