{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the hatch group (src/DXF/Entity/Hatch.hs); fixtures from tools/gen_hatch.py.
module Check.Hatch (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "hatch" ["test/data/hatch.dxf", "test/data/hatch_r2000.dxf"] ["HATCH"]
