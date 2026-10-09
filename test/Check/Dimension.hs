{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the dimension group (owner: whoever owns src/DXF/Entity/Dimension.hs).
module Check.Dimension (checks) where

import Check.Group
import Check.Util

-- | Fixtures come from tools/gen_dimension.py.
checks :: [(String, IO Result)]
checks = []  -- groupChecks "dimension" ["test/data/dimension.dxf", "test/data/dimension_r12.dxf"] ["TYPE", ...]
