{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the objects group (owner: whoever owns src/DXF/Entity/Objects.hs).
module Check.Objects (checks) where

import Check.Group
import Check.Util

-- | Fixtures come from tools/gen_objects.py.
checks :: [(String, IO Result)]
checks = []  -- groupChecks "objects" ["test/data/objects.dxf", "test/data/objects_r12.dxf"] ["TYPE", ...]
