{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the text group (owner: whoever owns src/DXF/Entity/Text.hs).
module Check.Text (checks) where

import Check.Group
import Check.Util

-- | Fixtures come from tools/gen_text.py.
checks :: [(String, IO Result)]
checks = []  -- groupChecks "text" ["test/data/text.dxf", "test/data/text_r12.dxf"] ["TYPE", ...]
