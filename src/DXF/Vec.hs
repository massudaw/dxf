-- | Points and vectors: strict, unboxed, no dependencies.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module DXF.Vec
  ( V2 (..), V3 (..), v3zero, v2zero, toV3, xy
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data V2 = V2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Ord, Show, Generic, NFData)

data V3 = V3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Ord, Show, Generic, NFData)

v2zero :: V2
v2zero = V2 0 0

v3zero :: V3
v3zero = V3 0 0 0

-- | Lift a 2D point to 3D with z = 0.
toV3 :: V2 -> V3
toV3 (V2 x y) = V3 x y 0

-- | Drop z.
xy :: V3 -> V2
xy (V3 x y _) = V2 x y
