{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the misc group (src/DXF/Entity/Misc.hs); fixtures from tools/gen_misc.py.
module Check.Misc (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "misc" ["test/data/misc.dxf", "test/data/misc_r2010.dxf"]
  ["VIEWPORT", "IMAGE", "WIPEOUT", "PDFUNDERLAY", "3DSOLID", "BODY", "REGION"]
