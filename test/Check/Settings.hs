{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the settings group (owner: whoever owns src/DXF/Entity/Settings.hs).
module Check.Settings (checks) where

import Check.Group
import Check.Util

-- | Fixtures come from tools/gen_settings.py.
checks :: [(String, IO Result)]
checks = []  -- groupChecks "settings" ["test/data/settings.dxf", "test/data/settings_r12.dxf"] ["TYPE", ...]
