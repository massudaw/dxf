{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the text group (src/DXF/Entity/Text.hs); fixtures from tools/gen_text.py.
module Check.Text (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "text" ["test/data/text.dxf", "test/data/text_r12.dxf"]
  ["TEXT", "ATTRIB", "ATTDEF", "MTEXT", "TOLERANCE", "SHAPE"]
