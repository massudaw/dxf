-- | Points and vectors: strict, unboxed, no dependencies.
{-# OPTIONS_GHC -Wno-orphans #-}
module DXF.Vec
  ( V2 (..), V3 (..), v3zero, v2zero, toV3, xy
  ) where

import Control.DeepSeq (NFData (..))
import Geometry.Core.Vec (V2 (..), V3 (..))

-- | The types are geometry-core's; the instances stay here (the fields are strict and unpacked, so there is nothing to force).
instance NFData V2 where rnf (V2 _ _) = ()

instance NFData V3 where rnf (V3 _ _ _) = ()

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
