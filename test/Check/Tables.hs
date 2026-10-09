{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the tables group (owner: whoever owns src/DXF/Entity/Tables.hs).
module Check.Tables (checks) where

import Check.Group
import Check.Util

-- | Fixtures come from tools/gen_tables.py.
checks :: [(String, IO Result)]
checks = []  -- groupChecks "tables" ["test/data/tables.dxf", "test/data/tables_r12.dxf"] ["TYPE", ...]
