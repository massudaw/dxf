{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the dimension group (src/DXF/Entity/Dimension.hs); fixtures from tools/gen_dimension.py.
module Check.Dimension (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "dimension" ["test/data/dimension.dxf", "test/data/dimension_r12.dxf"] ["DIMENSION", "LEADER"]
