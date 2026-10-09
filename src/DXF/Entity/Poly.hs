{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Polylines, splines, inserts: LWPOLYLINE, POLYLINE, VERTEX, SEQEND, SPLINE, HELIX, MESH, MLINE, INSERT.
module DXF.Entity.Poly
  ( PolyBody (..), polyCodecs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import DXF.Codec

-- | Placeholder until the group is written.
data PolyBody = PolyBodyStub
  deriving (Eq, Show, Generic, NFData)

polyCodecs :: [BodyCodec PolyBody]
polyCodecs = []
