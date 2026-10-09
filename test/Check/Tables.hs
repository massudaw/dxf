{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the table entries (src/DXF/Entity/Tables.hs); fixtures from tools/gen_tables.py.
module Check.Tables (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "tables" ["test/data/tables.dxf", "test/data/tables_r12.dxf"]
  ["LAYER", "LTYPE", "STYLE", "DIMSTYLE", "VPORT", "VIEW", "UCS", "APPID", "BLOCK_RECORD"]
