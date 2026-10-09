{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the settings objects (src/DXF/Entity/Settings.hs); fixtures from tools/gen_objects.py.
module Check.Settings (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "settings" ["test/data/objects.dxf"] ["LAYOUT", "PLOTSETTINGS", "MLINESTYLE"]
