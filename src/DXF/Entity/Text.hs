{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Text entities: TEXT, ATTRIB, ATTDEF, MTEXT, TOLERANCE, SHAPE.
module DXF.Entity.Text
  ( TxtBody (..), txtCodecs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec

-- | Placeholder until the group is written.
data TxtBody = TxtBodyStub
  deriving (Eq, Show, Generic, NFData)

txtCodecs :: [BodyCodec TxtBody]
txtCodecs = []
