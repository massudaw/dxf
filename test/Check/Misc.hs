{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the misc group (owner: whoever owns src/DXF/Entity/Misc.hs).
module Check.Misc (checks) where

import Check.Group
import Check.Util

-- | Fixtures come from tools/gen_misc.py.
checks :: [(String, IO Result)]
checks = []  -- groupChecks "misc" ["test/data/misc.dxf", "test/data/misc_r12.dxf"] ["TYPE", ...]
