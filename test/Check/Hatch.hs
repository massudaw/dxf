{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the hatch group (owner: whoever owns src/DXF/Entity/Hatch.hs).
module Check.Hatch (checks) where

import Check.Group
import Check.Util

-- | Fixtures come from tools/gen_hatch.py.
checks :: [(String, IO Result)]
checks = []  -- groupChecks "hatch" ["test/data/hatch.dxf", "test/data/hatch_r12.dxf"] ["TYPE", ...]
